(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.command
    (:documentation "Interactive commands' core")
  (:use :cl #:breeze.logging #:breeze.workspace)
  (:import-from #:alexandria
                #:symbolicate
                #:with-gensyms
                #:if-let
                #:when-let
                #:lastcar
                #:when-let*)
  (:import-from #:breeze.utils
                #:before-last)
  (:import-from #:breeze.string
                #:symbol-package-qualified-name)
  (:import-from #:breeze.indirection
                #:indirection)
  (:import-from #:breeze.channel
                #:channel
                #:make-channel
                #:emptyp)
  (:import-from #:breeze.buffer
                #:name)
  (:export
   #:start-command
   #:cancel-command
   #:continue-command
   #:deregister
   ;; Functions to access the context
   #:context
   #:context*
   #:context-get
   #:context-set
   #:current-buffer
   #:current-buffer-name
   #:current-buffer-filename
   #:current-point
   #:current-point-min
   #:current-point-max
   #:current-node-iterator
   ;; Basic composables commands
   #:insert
   #:read-string
   #:choose
   #:read-string-then-insert
   #:insert-at
   #:insert-at-saving-excursion
   #:replace-region
   #:message
   #:find-file
   #:ask-y-or-n-p
   #:return-value-from-command
   #:return-from-command
   ;; Command discovery and documentation
   #:commandp
   #:list-all-commands
   #:describe-command
   #:command-docstring
   #:command-description
   ;; Utilities to create commands
   #:define-command))

(in-package #:breeze.command)


;;; Actors

(defvar *actors* (make-hash-table)
  "Collection of command-handlers")

(defvar *actor-id-counter* 0
  "Counter used to generate ids for actors")

;; (setf *actor-id-counter*  0)

(defvar *actors-lock* (bt:make-lock)
  "Mutex for *actors*")

(defun generate-actor-id ()
  (bt:with-lock-held (*actors-lock*)
    (loop
      for id = (incf *actor-id-counter*)
      unless (gethash id *actors*)
        do (setf (gethash id *actors*) t)
           (return id))))

(defun register (actor)
  (bt:with-lock-held (*actors-lock*)
    (setf (gethash (id actor) *actors*) actor)))

(defun deregister (actor)
  (bt:with-lock-held (*actors-lock*)
    (remhash (if (numberp actor) actor (id actor)) *actors*)))

(defun find-actor (id &key errorp)
  (check-type id integer)
  (let ((actor (bt:with-lock-held (*actors-lock*)
                 (gethash id *actors*))))
    (when (and errorp (null actor))
      (error "Failed to find command with id ~S." id))
    actor))

;; TODO gabage collect the *actors* that are done (when?)
;; TODO How to detect if something went wrong?

#++
(bt:with-lock-held (*actors-lock*)
  (maphash (lambda (id actor)
             (when (or (donep actor)
                       (not (bt:thread-alive-p (thread actor))))
               (remhash id *actors*))
             ;; (break "~s ~s" id actor)
             )
           *actors*))

#++
(hash-table-count *actors*)

(defun clear-actors ()
  "Forget all the actors"
  (bt:with-lock-held (*actors-lock*)
    (clrhash *actors*)))

;; TODO rename to "actor"
;; TODO move to thread.lisp
(defclass command-handler ()
  ((id
    :initform (generate-actor-id)
    :accessor id
    :documentation "The id of the actor.")
   (fn
    :initform nil
    :initarg :fn
    :accessor fn
    :documentation "The function to be executed by the actor.")
   (created-at
    :initform (get-internal-real-time)
    :accessor created-at
    :documentation "When (universal-time0 was this actor created?")
   (thread
    :initform nil
    :initarg :thread
    :accessor thread
    :documentation "The thread running the actor.")
   (channel-in
    :initform (make-channel)
    :accessor channel-in
    :type channel
    :documentation "The channel to send response to the actor.")
   (channel-out
    :initform (make-channel)
    :accessor channel-out
    :type channel
    :documentation "The channel to receive requests from the actor.")
   (context
    :initarg :context
    :initform nil
    :accessor context
    :documentation "The context of the command."))
  (:documentation "Contains the runtime data used by the actor."))

(defmethod initialize-instance :after ((command command-handler) &key)
  (register command))

(defun make-command-handler (fn context-plist)
  (let* ((buffer (and context-plist
                      (add-to-workspace context-plist))))
    (make-instance
     'command-handler
     :fn fn
     :context (alexandria:alist-hash-table
               (when buffer
                 `((:buffer . ,buffer)))))))

(defmethod print-object ((command command-handler) stream)
  (print-unreadable-object
      (command stream :type t :identity nil)
    (let* ((then (created-at command))
                   (now (get-internal-real-time))
                   (diff (- now then))
                   (seconds (/ diff internal-time-units-per-second)))
              (multiple-value-bind (m s) (floor seconds 60)
                (format stream "~s (~dm ~ds ago)" (fn command)
                        m (ceiling s))))))

(defmethod thread :around ((_ (eql nil))) nil)


;;; Channels

(defmethod channel-in :around (command)
  "Helper method for easier debugging."
  (let ((result (call-next-method)))
    (unless result
      (error "(channel-in ~s) returned nil" command))
    result))

(defmethod channel-out :around (command)
  "Helper method for easier debugging."
  (let ((result (call-next-method)))
    (unless result
      (error "(channel-out ~s) returned nil" command))
    result))

(defmethod channel-in ((_ (eql nil)))
  "Helper method for easier debugging."
  (declare (ignore _))
  (warn "channel-in called on nil.")
  nil)

(defmethod channel-out ((_ (eql nil)))
  "Helper method for easier debugging."
  (declare (ignore _))
  (warn "channel-out called on nil.")
  nil)


(define-condition stop ()
  ()
  (:documentation "Condition used to stop the current command."))

(defun return-from-command ()
  "Signal a condition of type STOP to stop a command."
  (signal 'stop))

;; TODO maybe merge with cancel-command-on-error
(defun call-with-command-signal-handler (fn)
  "Establishes a throw tag named STOP to non-locally stop a command. Also
installs a condition handler for conditions of type STOP, the handler
uses the throw tag to stop the command immediately."
  (catch 'stop
    (handler-bind
        ((stop (lambda (condition)
                 (declare (ignore condition))
                 (throw 'stop nil))))
      (funcall fn))))

(defun %recv (channel)
  (let ((value (breeze.channel:receive channel)))
    (when (eq value 'stop)
      (return-from-command))
    ;; Return from command uses cl:signal, which means that it'll do
    ;; nothing if there's no handler setup. So we always return the
    ;; value.
    value))

(defun %send (channel value)
  (breeze.channel:send channel value)
  value)


(defmethod send-into ((command command-handler) value)
  (%send (channel-in command) value))

(defmethod recv-from ((command command-handler))
  (%recv (channel-out command)))

(defmethod recv-into ((command command-handler))
  (%recv (channel-in command)))

(defmethod send-out ((command command-handler) value)
  (%send (channel-out command) value))

(defvar *command* nil
  "The command that is currently being executed.")

(defun recv ()
  "Meant to be used by the commands."
  (recv-into *command*))

(defun recv1 ()
  "Meant to be used by the commands."
  (car (recv)))

(defun send (request &rest data)
  "Meant to be used by the commands."
  (send-out *command* `(,request ,@data)))


(defun stop-actor (actor)
  (when-let* ((thread (thread actor)))
    (handler-case
        (progn
          ;; Ask very gently
          (send-into actor 'stop)
          (sleep 0.001)
          ;; Ask gently
          (loop :repeat 100
                :while (bordeaux-threads:thread-alive-p thread)
                :do (bt:interrupt-thread thread 'return-from-command)
                    (sleep 0.001))
          (unless (bordeaux-threads:thread-alive-p thread)
            ;; Kill the associated thread.
            (bt:destroy-thread thread)))
      ;; This is signaled when interrupting a thread fails because the
      ;; thread is not alive. (p.s. on sbcl, both bt:interrupt-thread
      ;; and bt:destroy-thread ends up interrupting the thread)
      #+sbcl
      (sb-thread:interrupt-thread-error (condition)
        (declare (ignore condition)))
      #+ecl
      (error (condition)
        (declare (ignore condition))))))

;; TODO rename cancel -> stop
(defun cancel-command (id &optional reason)
  "Cancel a command."
  (let ((actor (find-actor id :errorp t)))
    (stop-actor actor)
    (if reason
        (log-debug "Command canceled because ~a." reason)
        (log-debug "Command canceled for unspecified reason.")))
  ;; Return nil
  nil)


(defun outgoing-messages-p (command)
  ;; There are no outgoing messages waiting.
  ;; blockp = no message
  (not (emptyp (channel-out command))))

(defun thread-dead-p (command)
  (and (thread command)
       ;; The thread is finisheds
       (null (bt:thread-alive-p (thread command)))))

(defun donep (command)
  (if command
      ;; Command is considered done because it has a thread and it is
      ;; not alive, and there is not outbound messages wainting.
      (and
       (thread-dead-p command)
       (not (outgoing-messages-p command)))
      ;; *command* was probably set to nil
      ;; "Command is considered done because it is null."
      t))

;; TODO maybe merge with call-with-command-signal-handler
;;
;; TODO add some kind of per-command circuit-breaker, because when
;; something well integrated breaks (e.g. hooks on edits, on-the-fly
;; linter, etc) the editor becomes unusable. Will need to add an
;; argument `command-function'
(defun cancel-command-on-error (id thunk)
  (tagbody
   :retry
     (restart-case
         (handler-bind
             ((error #'(lambda (condition)
                         (log-error "~&An error occurred: ~a" condition)
                         (cancel-command id condition))))
           (funcall thunk))
       (retry-command ()
         :report "Retry calling the command"
         (go :retry)))))

(defun maybe-variables (package-name &rest symbol-names)
  (if-let ((package (find-package package-name)))
    (loop :for symbol-name :in symbol-names
          :for symbol = (find-symbol symbol-name package)
          :when (and symbol (boundp symbol))
            :collect (cons symbol (symbol-value symbol)))))

;; There are packages, like trivial-indent, that will call swank (or
;; slynk), but swank will signal an error because the symbol
;; swank::*emacs-connection* and swank::send-counter are bound to nil
;; inside the new threads.
(defun maybe-swank-special-variables ()
  ;; TODO this might not work if readtable-case is not :upcase
  (maybe-variables "SWANK"
                   "*EMACS-CONNECTION*"
                   "*SEND-COUNTER*"))

(defun maybe-slynk-special-variables ()
  ;; TODO this might not work if readtable-case is not :upcase
  (maybe-variables "SLYNK"
                   "*EMACS-CONNECTION*"
                   "*SEND-COUNTER*"))

#++ (maybe-swank-special-variables)
#++ (maybe-slynk-special-variables)

(defun make-actor-thread (actor fn)
  "Initialize a actor's thread."
  (let* ((*command* actor)
         (thread
           (bt:make-thread
            fn
            :name "breeze command handler"
            :initial-bindings
            `((*package* . ,*package*)
              (*command* . ,actor)
              ,@(maybe-swank-special-variables)
              ,@(maybe-slynk-special-variables)
              ,@bt:*default-special-bindings*))))
    (setf (thread actor) thread)
    actor))

(defun check-special-variables ()
  (unless *command*
    (error "In start-command: *command* is null"))
  (unless (context *command*)
    (error "In start-command: *command*'s context is null")))

(defun wait-for-sync (command)
  ;; Waiting for a message from start-command
  (log-debug "Waiting for 'sync message")
  (let ((sync-message (recv-into command)))
    (unless (eq 'sync sync-message)
      (error "In start-command: expected to receive the datum 'sync, received ~S instead"
             sync-message))
    (log-debug "Received 'sync message")))

(defun send-sync (command)
  ;; Send a message to the thread for synchronisation.
  (log-debug "Sending 'sync message...")
  (send-into command 'sync)
  (log-debug "'sync message sent"))

(defun wait-for-started-message (command)
  (log-debug "Waiting for the thread to be up and running...")
  ;; Wait for the thread to be up and running
  (let ((started-message (recv-from command)))
    (unless (eq 'started started-message)
      (error "In start-command: expected to receive the datum 'started, received ~S instead"
             started-message))))

(defun send-started-message (command)
  ;; Sending back a message to tell start-command the command is going to do its job
  (send-out command 'started))

(defun start-command (fn context-plist &optional extra-args)
  "Start processing a command, return its id"
  (log-debug "Starting command...")
  (check-type fn (or function symbol))
  (check-type context-plist (or null cons))
  (let* ((command (make-command-handler fn context-plist))
         (buffer (current-buffer (context command))))
    ;; Create the thread for the command handler
    (make-actor-thread
     command
     (lambda ()
       (cancel-command-on-error
        (id command)
        (lambda ()
          (call-with-command-signal-handler
           (lambda ()
             (check-special-variables)
             (wait-for-sync command)
             (send-started-message command)
             ;; TODO not all commands would require the buffer's content,
             ;; move this in a function that can be called on demand.
             ;;
             ;; TODO this assumes we have incremental parsing, which we
             ;; don't...
             (when (and buffer
                        (not (breeze.lossless-reader:node-iterator buffer)))
               (send "buffer-string")
               (let ((buffer-string (recv1)))
                 (breeze.buffer:update-buffer-content buffer buffer-string)))
             (apply fn extra-args)))))))
    (send-sync command)
    (wait-for-started-message command)
    (log-debug "Command started.")
    (id command)))

;; TODO Maybe rename ARGUMENTS to RESPONSE?
(defun continue-command (id &rest response)
  "Continue procressing *command*."
  ;; TODO cancel-command-on-error
  (let ((actor (find-actor id :errorp t)))
    (cond
      ((donep actor) (deregister actor) (list "done"))
      (t
       ;; TODO It would be nice to keep track of whether a response
       ;; is expected or not.
       (when response (send-into actor response))
       (let ((request (recv-from actor)))
         (cond
           ((null request)
            (cancel-command id "Request is null."))
           ((string= "done" (car request))
            (cancel-command id "Request is \"done\".")))
         request)))))


;;; Utilities to get common stuff from the context

(defun context* ()
  "Get the *command*'s context."
  (if *command*
      (context *command*)
      (error "*command* is nil")))

(defun context-set (context key value)
  "Set KEY to VALUE in CONTEXT."
  (setf (gethash key context) value))

(defun context-get (context key)
  "Get the value of KEY CONTEXT."
  (multiple-value-bind (value presentp)
      (gethash key context)
    (if presentp
        value
        (let* ((caughtp t)
               (value
                 (catch 'indirection
                   (signal 'indirection :form `(context-get ,key))
                   (setf caughtp nil))))
          (when caughtp
            (context-set context key value))
          value))))

(defun current-buffer (&optional (context (context*)))
  "Get the buffer from the CONTEXT.
It can be null."
  (context-get context :buffer))

(defun current-buffer-name (&optional (context (context*)))
  "Get the buffer's name from the CONTEXT.
It can be null."
  (when-let ((buffer (current-buffer context)))
    (name buffer)))

(defun current-buffer-filename (&optional (context (context*)))
  "Get the buffer's filename from the CONTEXT.
The buffer-filename is the name of the file that the buffer is
visiting.
It can be null."
  (when-let ((buffer (current-buffer context)))
    (filename buffer)))

(defun current-point (&optional (context (context*)))
  "Get the buffer's point from the CONTEXT.
The point is the position of the cursor.
It can be null."
  (when-let ((buffer (current-buffer context)))
    (point buffer)))

(defun current-point-min (&optional (context (context*)))
  "Get the buffer's point-min from the CONTEXT.
The point-min is the position of the beginning of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (when-let ((buffer (current-buffer context)))
    (point-min buffer)))

(defun current-point-max (&optional (context (context*)))
  "Get the buffer's point-max from the CONTEXT.
The point-max is the position of the end of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (when-let ((buffer (current-buffer context)))
    (point-max buffer)))

(defun current-node-iterator (&optional (context (context*)))
  (when-let ((buffer (current-buffer context)))
    (node-iterator buffer)))


;;; Basic commands, to be composed

;; This is mostly to work around window's end of lines...
(defun format* (control-string args)
  (apply #'format nil
         #.(if (uiop:os-windows-p)
               `(remove #\Return control-string)
               'control-string)
         args))

(defun insert (control-string &rest args)
  "Send a message to the editor telling it to insert STRING at
POSITION. Set SAVE-EXCURSION-P to non-nil to keep the current
position."
  (send
   "insert"
   (format* control-string args)))

(defun read-string (prompt &optional initial-input)
  "Send a message to the editor to ask the user to enter a string."
  (send "read-string" prompt initial-input)
  (recv1))

(defun read-string-then-insert (prompt control-string &optional (fn #'identity))
  (insert control-string
          (funcall fn (read-string prompt))))

(defun choose (prompt collection &key (allow-empty-collection-p t))
  "Send a message to the editor to ask the user to choose one element
in the collection. The user can also enter a value not in the
collection."
  (check-type collection list)
  ;; TODO Not sure yet if this check should be optional or not.
  ;; Check that the list of choice is not empty
  (when (and (null collection)
             (not allow-empty-collection-p))
    (error "The list of choices is empty."))
  (send "choose" prompt collection)
  (recv1))

(defun insert-at (position control-string &rest args)
  "Send a message to the editor telling it to insert STRING at
POSITION. Set SAVE-EXCURSION-P to non-nil to keep the current
position."
  (send "insert-at" position (format* control-string args)))

(defun insert-at-saving-excursion (position control-string &rest args)
  "Send a message to the editor telling it to insert STRING at
POSITION. Set SAVE-EXCURSION-P to non-nil to keep the current
position."
  (send "insert-at-saving-excursion"
        position
        (format* control-string args)))

;; TODO use format*, add control-string &rest args
;; TODO add a version that saves excursion
(defun replace-region (position-from position-to
                       replacement-string)
  "Send a message to the editor telling it to replace the text between
POSITION-FROM POSITION-TO by REPLACEMENT-STRING. Set SAVE-EXCURSION-P
to non-nil to keep the current position."
  (send
   "replace"
   position-from
   position-to
   replacement-string))

(defun message (control-string &rest format-arguments)
  "Send a message to the editor to ask it to show a message to the
user. This function pass its arguments to cl:format and sends the
resulting string to the editor."
  (send "message"
        (format* control-string format-arguments)))

(defun find-file (pathname)
  "Send a message to the editor to tell it to open the PATHNAME."
  (send "find-file"
        (namestring pathname)))

(defun ask-y-or-n-p (prompt &rest format-arguments)
  "Ask the user a y/n question."
  (loop :for answer = (read-string (format* prompt format-arguments))
        :for valid-p = (member answer '("y" "n") :test #'string-equal)
        :for i :below 3 ; guard against infinite loop
        :while (not valid-p)
        :finally (return (string-equal "y" answer))))

(defun return-value-from-command (value)
  (send "return" value))


;;; Command discovery and documentation

(defun commandp (symbol)
  (get symbol 'breeze.command::commandp))

(defun list-all-commands (&optional with-details-p)
  (loop
    :for package :in (list-all-packages)
    #++ (breeze.xref:find-packages-by-prefix "breeze.")
    :append (loop
              :for symbol :being :each :external-symbol :of package
              :for lambda-list-or-t = (commandp symbol)
              :when lambda-list-or-t
                :collect (if with-details-p
                             (list symbol (if (eq t lambda-list-or-t)
                                              nil
                                              lambda-list-or-t)
                                   (documentation symbol 'function))
                             symbol))))

(defgeneric describe-command (command)
  (:documentation "Give a user-friendly description for a command.")
  (:method ((command symbol))
    (symbol-package-qualified-name command)))

(defun command-docstring (function)
  "Return the function's docstring, signals an error if it's nil."
  (let ((doc (documentation function 'function)))
    (unless doc
      (error
       "Function ~s does not have a documentation string.~
                  Is it defined?"
       function))
    doc))

(defun command-description (function)
  "Return a list of 2 elements: (FUNCTION its-docstring)"
  (list function (command-docstring function)))


;;; Utilities to help creating commands less painful.

(defmacro define-command (name lambda-list
                          &body body)
  "Macro to define a command.

Example:

(define-command hi ()
  (message \"Hi ~a\" (read-string \"What is your name?\")))
"
  (multiple-value-bind (remaining-forms declarations docstring)
      (alexandria:parse-body body :documentation t)
    (check-type docstring string
                "Docstring are mandatory for commands")
    (multiple-value-bind (command-declarations cl-declarations)
        (loop :for (_ specifier) :in declarations
              :for identifier = (car specifier)
              :if (member identifier '(context))
                :collect specifier :into command-declarations
              :else
                :collect specifier :into cl-declarations
              :finally (return (values command-declarations cl-declarations)))
      `(values (prog1
                   (defun ,name ,lambda-list
                     ;; Add the users' declarations
                     ,@cl-declarations
                     ,docstring
                     (progn ,@remaining-forms)
                     (send "done"))
                 ;; Add flags into the symbol's plist
                 (setf (get ',name 'commandp) ',(or lambda-list t))
                 ,@(loop :for (identifier . rest) :in command-declarations
                         :for args = (cond
                                       ((null rest) t)
                                       ((null (cdr rest)) (car rest))
                                       (t rest))
                         :collect `(setf (get ',name ',identifier) ',args)))
               ',command-declarations))))
