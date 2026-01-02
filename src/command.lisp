(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.command
    (:documentation "Interactive commands' core")
  (:use :cl
        #:breeze.logging
        #:breeze.actor
        #:breeze.workspace)
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
                #:name
                #:node-iterator)
  (:export
   #:*command*
   #:send-into
   #:recv-from
   #:recv-into
   #:send-out)
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
   #:insert-saving-excursion
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
   #:request-buffer-string
   #:goto-char
   #:pulse
   ;; Command discovery and documentation
   #:commandp
   #:list-all-commands
   #:list-all-commands-for-editor
   #:describe-command
   #:command-docstring
   #:command-description
   ;; Utilities to create commands
   #:define-command
   #:noninteractive
   #:interactivep
   ;; Utilities to send notifications to the editor
   #:notify-editor
   #:notifications))

(in-package #:breeze.command)

(defclass command-handler (actor)
  ((context
    :initarg :context
    :initform nil
    :accessor context
    :documentation "The context of the command."))
  (:documentation "Contains the runtime data used by the command."))

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


;;;; Channels

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
  "Establishes a throw tag named `stop' to non-locally stop a
command. Also installs a condition handler for conditions of type
`stop', the handler
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
       ;; The thread is finished
       (null (bt:thread-alive-p (thread command)))))

(defun donep (command)
  (if command
      ;; Command is considered done because it has a thread and it is
      ;; not alive, and there is not outbound messages waiting.
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
  (declare (ignorable id))
  (tagbody
   :retry
     (restart-case
         (funcall thunk)
       ;; for breeze interactive-eval, I need the errors to
       ;; propagate...
       #++
       (handler-case (funcall thunk)
         (error (condition)
           (let ((actor (find-actor id :errorp t)))
             (log-error "~&An error occurred in ~a:~%  ~a" actor condition))
           ;; TODO print the backtrace if possible
           (cancel-command id condition)
           (send "done")))
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
             ;; TODO use "call-with-correction-suggestion" here too!
             (apply fn extra-args)))))))
    (send-sync command)
    (wait-for-started-message command)
    (log-debug "Command started.")
    (id command)))

;; TODO Maybe rename ARGUMENTS to RESPONSE?
(defun continue-command (id &key request-type response updates)
  "Continue procressing *command*."
  ;; TODO cancel-command-on-error
  (destructuring-bind (&key point edits &allow-other-keys)
      updates
    (let ((actor (find-actor id :errorp t)))
      (cond
        ((donep actor)
         (deregister actor)
         (list "done"))
        (t
         ;; TODO dispatch on request-type
         (format *trace-output* "~&~s updates: ~s" (fn actor) updates)
         ;; TODO extract handle-updates?
         ;; TODO extract handle-point
         ;; Update the current-buffer's point
         (when point
           (when-let ((buffer (current-buffer (context actor))))
             (breeze.buffer:update-point buffer point)))
         (format *trace-output* "~&~s current-source before:~%~s" (fn actor) (breeze.parser:source (current-buffer (context actor))))
         ;; TODO extract handle-edits
         (when edits
           (note-edits *workspace* edits)
           (apply-pending-edits *workspace*))
         (format *trace-output* "~&~s current-source after:~%~s" (fn actor) (breeze.parser:source (current-buffer (context actor))))
         ;; TODO It would be nice to keep track of whether a response
         ;; is expected or not.
         (when response (send-into actor response))
         (let ((request (recv-from actor)))
           (cond
             ((null request)
              (cancel-command id "Request is null."))
             ((string= "done" (car request))
              (cancel-command id "Request is \"done\".")))
           request))))))


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

(defun (setf current-buffer) (buffer &optional (context (context*)))
  "Set the buffer from the CONTEXT.
It can be null."
  (context-set context :buffer buffer))

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


;;; Primitive commands, to be composed

;; This is mostly to work around window's end of lines...
(defun format* (control-string args)
  (apply #'format nil
         #.(if (uiop:os-windows-p)
               `(remove #\Return control-string)
               'control-string)
         args))

;; TODO rename insert-format
;; TODO add a method named `insert'
(defun insert (control-string &rest args)
  "Send a message to the editor telling it to insert STRING at
POSITION."
  (send
   "insert"
   (format* control-string args)))

(defun insert-saving-excursion (control-string &rest args)
  "Send a message to the editor telling it to insert STRING at
POSITION, saving and restoring the current position."
  (send
   "insert-saving-excursion"
   (format* control-string args)))

(defun history-name (history)
  "Get the name. as a string, of the variable used to store the
history. Defaults to the name of the current command if HISTORY is
nil. Prepends th prefix \"breeze-\" if necessary."
  (breeze.string:ensure-prefix
   "breeze-"
   (format nil "~(~a~@[--~a~]~)"
           (fn *command*) history)))

(defun read-string (prompt &key initial-input history)
  "Send a message to the editor to ask the user to enter a string."
  (send "read-string" prompt
        :initial-input initial-input
        :history (history-name history))
  (recv))

(defun read-string-then-insert (prompt control-string &optional (fn #'identity))
  (insert control-string
          (funcall fn (read-string prompt))))

(defun choose (prompt collection
               &key
                 (allow-empty-collection-p t)
                 ;; TODO :select-1
                 ;; TODO :require-match
                 history)
  "Send a message to the editor to ask the user to choose one element
in the collection. The user can also enter a value not in the
collection."
  (check-type collection list)
  ;; TODO Not sure yet if this check should be optional or not.
  ;; Check that the list of choice is not empty
  (when (and (null collection)
             (not allow-empty-collection-p))
    (error "The list of choices is empty."))
  (send "choose" prompt collection
        :history (history-name history))
  (recv))

(defun insert-at (position control-string &rest args)
  "Send a message to the editor telling it to insert STRING at
POSITION."
  (send "insert-at" position (format* control-string args)))

(defun insert-at-saving-excursion (position control-string &rest args)
  "Send a message to the editor telling it to insert STRING at
POSITION, saving and restoring the current position."
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

(defun find-file (pathname &optional other-window-p)
  "Send a message to the editor to tell it to open the PATHNAME."
  (send "find-file"
        (namestring pathname) other-window-p))

(defun ask-y-or-n-p (prompt &rest format-arguments)
  "Ask the user a y/n question."
  (loop :for answer = (read-string (format* prompt format-arguments))
        :for valid-p = (member answer '("y" "n") :test #'string-equal)
        :for i :below 3 ; guard against infinite loop
        :while (not valid-p)
        :finally (return (string-equal "y" answer))))

(defun return-value-from-command (value)
  (send "return" value))

(defun request-buffer-string ()
  (let ((buffer (current-buffer)))
    (send "buffer-string")
    (let ((buffer-string (recv)))
      (format *trace-output*
              "~&got buffer-string:~%~s" buffer-string)
      (breeze.buffer:update-content buffer
                                    buffer-string
                                    (current-point)))))

(defun goto-char (position)
  "Move the point to POSITION."
  (send "goto-char" position))

(defun pulse (start end)
  "Momentarily pulse (highlight then fade) the region from START to END."
  (send "pulse" start end))


;;; Command discovery and documentation

;; TODO keep a list of removed commands?
;;  - hook into "unregister-comman"
;;   - don't forget to remove a command from the list if it's registered
;;     again
;; It's still possible for someone to `fmakunbound' a command without
;; deregistering it. We should try to detect that as well.
;; There's _also" the case where someone `unexport' the function...

(defclass command-set ()
  ((commands
    :initform (make-hash-table)
    :initarg :commands
    :reader commands
    :documentation "Set of commands.")
   (timestamp
    :initform nil
    :initarg :timestamp
    :accessor timestamp
    :documentation "Timestamp that is updated every time a command is (re)defined. Used to
figure out if the editor is out of sync.")
   (lock
    :initform (bt2:make-lock :name "command-set lock")
    :initarg :lock
    :accessor lock
    :documentation "Mutex to access this set of commands."))
  (:documentation "A set of commands. Includes a mutex lock for thread-safe access and a
timestamp to help synchronisation (e.g. with an editor)."))

(defun %touch-commands (command-set)
  "Update an internal timestamp used by the editor(s) to know if any
commands where updated, addeded or removed since last time it
refreshed the list of commands."
  (setf (timestamp command-set) (get-universal-time)))

(defun register-command (command-set command details)
  "Register COMMAND in COMMAND-SET. DETAILS is expected to be a plist
containing at least the keys :lambda-list :documentation and :declarations."
  (check-type command-set command-set)
  (check-type command symbol)
  ;; validating the details
  (destructuring-bind (&key lambda-list documentation declarations)
      details
    (declare (ignore declarations))
    (check-type documentation string)
    (check-type lambda-list list))
  (bt2:with-lock-held ((lock command-set))
    (%touch-commands command-set)
    (setf (gethash command (commands command-set)) details)))

(defun unregister-command (command-set command)
  "Unregister COMMAND from COMMAND-SET."
  (check-type command symbol)
  (bt2:with-lock-held ((lock command-set))
    (%touch-commands command-set)
    (remhash command (commands command-set))))


;;; *Commands*

(defvar *commands* (make-instance 'command-set)
  "The set of all registered commands.")

(defun register-command* (command details)
  "Register COMMAND in the current command-set `*commands*'."
  (register-command *commands* command details))

(defun unregister-command* (command)
  "Unregister COMMAND from the current command-set `*commands*'."
  (unregister-command* command))

(defun find-command (command)
  "Find COMMAND in the current command-set `*commands*'."
  (check-type command symbol)
  (gethash command (commands *commands*)))

(defun commandp (command)
  "Test if COMMAND is a symbol that designate a function, that it is
registered in the current command-set `*commands*' and that it is
exported from its home package."
  (and
   (symbolp command)
   (fboundp command)
   (find-command command)
   (breeze.xref:externalp command)))

(defun interactivep (function-or-declarations)
  "Return true if a command's declaration doesn't contain breeze.command:noninteractive."
  (if (listp function-or-declarations)
      (null (member 'noninteractive function-or-declarations))
      (interactivep (getf (find-command function-or-declarations) :declarations))))

(defun list-all-commands ()
  "Get the list of commands (list of symbols) registered in the current
command-set `*commands*'."
  (alexandria:hash-table-keys (commands *commands*)))


;;; Synchronising the list of commands with the editor

(defun command-name-for-editor (command)
  "Compute a name for COMMAND suitable for an editor. Returns a lowercase
string. prefixed with \"breeze-\"."
  (check-type command symbol)
  (breeze.string:ensure-prefix
   "breeze-"
   (string-downcase (symbol-name command))))

(defun command-lambda-list-for-editor (lambda-list)
  "Compute a lambda-list more suitable for an editor. Returns a list of
uninterned symbols."
  (loop :for el :in lambda-list
        :for sym := (etypecase el (cons (car el)) (symbol el))
        :for name := (symbol-name sym)
        :unless (member sym '(&optional &aux &key &allow-other-keys))
          :collect (make-symbol name)))

(defun list-all-commands-for-editor ()
  "Compute a list of command specifications suitable for an
editor. Returns a list of plist."
  (loop
    :for command-symbol :being :the :hash-key :of (commands *commands*) :using (hash-value details)
    :for documentation := (getf details :documentation)
    :for declarations := (getf details :declarations)
    :for lambda-list := (command-lambda-list-for-editor (getf details :lambda-list))
    :for name := (command-name-for-editor command-symbol)
    ;; The editor should only see the commands that are exported and
    ;; interactive.
    :when (and (breeze.xref:externalp command-symbol)
               (interactivep declarations))
      :collect (list
                :name (make-symbol (string-upcase name))
                :symbol (symbol-package-qualified-name command-symbol)
                :lambda-list lambda-list
                :documentation documentation)))

;; (list-all-commands-for-editor)

(defun command-docstring (function)
  "Return the function's docstring, signals an error if it's nil."
  ;; WIP find-command
  (check-type function symbol)
  (let ((doc (documentation function 'function)))
    (unless doc
      (error
       "Function ~s does not have a documentation string. ~
                  Is it defined?"
       function))
    doc))

(defun command-description (function)
  "Return a list of 2 elements: (FUNCTION its-docstring)"
  (list function (command-docstring function)))



;;; Utilities to help creating commands less painful.

#|
TODO list/document/design/validate custom declarations
 - (context where &key :extension)
   - where
     - :top-level
     - cl:defun
     - :loop-clause
     - :format-string
     - :expression
     - (funcall f) ... ?
     - (cl:defclass :slot-specifier)
     - skipp - t nil :whitespace :comment
   - context: first line, last line, bol, eol...
 - (interactive ...)

|#

(defmacro define-command (name lambda-list &body body)
  "Macro to define a command.

Example:

(define-command hi ()
  (message \"Hi ~a\" (read-string \"What is your name?\")))
"
  ;; Parse the body to correctly extract the docstring and the
  ;; declarations
  (multiple-value-bind (remaining-forms declarations docstring)
      (alexandria:parse-body body :documentation t)
    (check-type docstring string
                "Docstring are mandatory for commands")
    ;; Partition the declarations between "cl" declarations and
    ;; "command" declarations.
    (multiple-value-bind (command-declarations cl-declarations)
        (loop :for (_ specifier) :in declarations
              :for identifier = (if (listp specifier)
                                    (car specifier)
                                    specifier)
              ;; recognized declarations:
              :if (member identifier '(context noninteractive))
                :collect specifier :into command-declarations
              :else
                :collect specifier :into cl-declarations
              :finally (return (values command-declarations cl-declarations)))
      ;;
      (let ((recursive-p (intern (string :recursive-p))))
        `(values (prog1
                     ;; define
                     (defun ,name ,(or lambda-list
                                    `(&key ,recursive-p))
                       ;; Add the users' declarations
                       ,docstring
                       ,@(loop :for declaration :in cl-declarations
                               :collect `(declare ,declaration))
                       (multiple-value-prog1
                           (progn
                               ;; TODO not all commands would require
                               ;; the buffer's content
                             (request-buffer-string)
                             ,@remaining-forms)
                         ,(if lambda-list
                              `(send "done")
                              `(unless ,recursive-p (send "done")))))
                   ;; register
                   (register-command* ',name (list
                                              :lambda-list ',lambda-list
                                              :documentation ,docstring
                                              :declarations ',command-declarations)))
                 ',command-declarations)))))


;;; Utilities to send notifications to the editor

;; TODO what if there are multiple editors connected?
(defvar *notifications* (make-channel))

(defmethod notify-editor ((notification-channel channel) datum)
  (breeze.channel:send notification-channel datum))

#++
(define-command notifications ()
  "Receive all the notifications one-by-one, in a loop, waiting for new
ones if necessary."
  (loop :for datum := (breeze.channel:receive *notifications*)
        :do (send datum)))
