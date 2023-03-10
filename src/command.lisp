(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.command
    (:documentation "Interactive commands' core")
  (:use :cl)
  (:import-from #:alexandria
                #:symbolicate
                #:with-gensyms
                #:if-let
                #:lastcar
                #:when-let*)
  (:import-from #:breeze.reader
                #:parse-string)
  (:import-from #:breeze.syntax-tree
                #:find-path-to-node)
  (:import-from #:breeze.utils
                #:before-last)
  (:export
   #:start-command
   #:cancel-command
   #:continue-command
   ;; Functions to access the context
   #:context
   #:context*
   #:context-get
   #:context-set
   #:context-buffer-string
   #:context-buffer-string*
   #:context-buffer-name
   #:context-buffer-name*
   #:context-buffer-file-name
   #:context-buffer-file-name*
   #:context-point
   #:context-point*
   #:context-point-min
   #:context-point-min*
   #:context-point-max
   #:context-point-max*
   ;; Basic composables commands
   #:insert
   #:read-string
   #:choose
   #:read-string-then-insert
   #:insert-at
   #:replace-region
   #:backward-char
   #:message
   #:find-file
   #:ask-y-or-n-p
   ;; Utilities to create commands
   #:return-from-command
   #:define-command
   ;; Utilities to add very useful information into the context
   #:augment-context-by-parsing-the-buffer
   ;; Keys in the *context* hash-table
   #:buffer-string
   #:buffer-name
   #:buffer-file-name
   #:point
   #:point-min
   #:point-max
   #:point
   #:nodes
   #:path
   #:outer-node
   #:inner-node
   #:inner-node-index
   #:parent-node))

(in-package #:breeze.command)

(defclass command-handler ()
  ((thread
    :initform nil
    :initarg :thread
    :accessor thread
    :documentation "The thread that process the command.")
   (channel-in
    :initform (make-instance 'chanl:unbounded-channel)
    :accessor channel-in
    :type (or null chanl:channel)
    :documentation "The channel to send response to the thread.")
   (channel-out
    :initform (make-instance 'chanl:unbounded-channel)
    :accessor channel-out
    :type (or null chanl:channel)
    :documentation "The channel to receive requests from the thread.")
   (context
    :initarg :context
    :initform nil
    :accessor context
    :documentation "The context of the command."))
  (:documentation "Contains the runtime data to run a command."))

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
  ())

(defun return-from-command ()
  (signal 'stop))

(defun call-with-command-signal-handler (fn)
  (catch 'stop
    (handler-bind
        ((stop (lambda (condition)
                 (declare (ignore condition))
                 (throw 'stop nil))))
      (funcall fn))))

(defun %recv (channel)
  (let ((value (chanl:recv channel)))
    (when (eq value 'stop)
      (return-from-command))
    ;; Return from command uses cl:signal, which means that it'll do
    ;; nothing if there's no handler setup. So we always return the
    ;; value.
    value))

(defun %send (channel value)
  (chanl:send channel value))


(defmethod send-into ((command command-handler) value)
  (%send (channel-in command) value))

(defmethod recv-from ((command command-handler))
  (%recv (channel-out command)))

(defmethod recv-into ((command command-handler))
  (%recv (channel-in command)))

(defmethod send-out ((command command-handler) value)
  (%send (channel-out command) value))

;; No, I won't support multiple client/command at the same time, for
;; now™.
(defparameter *command* nil
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



;; TODO rename cancel -> stop

(defun cancel-command-from-inside ()
  (return-from-command))

(defun cancel-command-from-outside (command thread)
  (handler-case
      (progn
        ;; Ask very gently
        (send-into command 'stop)
        (sleep 0.001)
        ;; Ask gently
        (loop :repeat 100
              :while (bordeaux-threads:thread-alive-p thread)
              :do (bt:interrupt-thread thread 'return-from-command)
                  (sleep 0.001))
        (unless (bordeaux-threads:thread-alive-p thread)
          ;; Kill the associated thread.
          (bt:destroy-thread thread)
          (log:error "Had to..")))
    ;; This is signaled when interrupting a thread fails because the
    ;; thread is not alive. (p.s. on sbcl, both bt:interrupt-thread
    ;; and bt:destroy-thread ends up interrupting th thread)
    #+sbcl
    (sb-thread:interrupt-thread-error (condition)
      (declare (ignore condition)))))

;; (trace cancel-command-from-inside cancel-command-from-outside)

(defun cancel-command (&optional reason)
  "Cancel the currently running command."
  (when-let* ((command *command*)
              (thread (thread command)))
    (if (eq (bt:current-thread) thread)
        (cancel-command-from-inside)
        (cancel-command-from-outside command thread)))
  ;; Clear the special variable
  (setf *command* nil)
  ;; Log
  (if reason
      (log:debug "Command canceled because ~a." reason)
      (log:debug "Command canceled for unspecified reason."))
  ;; Return nil
  nil)


(defun outgoing-messages-p (command)
  ;; There are no outgoing messages waiting.
  ;; blockp = no message
  (not (chanl:recv-blocks-p (channel-out command))))

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
       (outgoing-messages-p command))
      ;; *command* was probably set to nil
      ;; "Command is considered done because it is null."
      t))

(defun command* ()
  "Helper to get the current command, or correctly set it to nil."
  (if *command*
      *command*
      (progn (cancel-command "Done") nil)))

(defun run-command (command arguments)
  "Receive request from the command's thread."
  (if (donep command)
      (progn
        ;; This is called mainly to set *command*
        (cancel-command "run-command: command was donep")
        (list "done"))
      (progn
        (when arguments (send-into command arguments))
        (let ((request (recv-from command)))
          (cond
            ((null request)
             (cancel-command "Request is null."))
            ((string= "done" (car request))
             (cancel-command "Request is \"done\".")))
          request))))

(defun call-with-cancel-command-on-error (thunk)
  (handler-bind
      ((error #'(lambda (condition)
                  ;; (declare (ignore condition))
                  (log:error "~&An error occurred: ~a" condition)
                  (cancel-command condition))))
    (funcall thunk)))

(defmacro cancel-command-on-error (&body body)
  `(call-with-cancel-command-on-error
    (lambda () ,@body)))

(defun context-plist-to-hash-table (context-plist)
  (loop
    :with ht = (make-hash-table)
    :for (key value) :on context-plist :by #'cddr
    :for normalized-key = (if (member (symbol-name key)
                                      '#.(mapcar #'symbol-name
                                                 '(buffer-string
                                                   buffer-name
                                                   buffer-file-name
                                                   point
                                                   point-min
                                                   point-max))
                                      :test #'string=)
                              (intern (symbol-name key) #.*package*)
                              key)
    :when value
      :do (setf (gethash normalized-key ht) value)
    :finally (return ht)))

;; (1) There are packages, like trivial-indent, that will call swank,
;; but swank will signal an error because the symbol
;; swank::*emacs-connection* and swank::send-counter are bound to nil.
;;
;; Furthermore... I need to make my macro work even if swank is not
;; loaded, so I made this workaround... Hopefully it won't break
;; anyone's code 'o_o
;;
;; TODO Sly probably needs the same workaround
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "SWANK")
    (make-package "SWANK")))
(defvar swank::*emacs-connection* nil)
(defvar swank::*send-counter* nil)

(defmacro make-command-thread ((command-handler) &body body)
  "Initialize a command-handler's thread."
  (alexandria:once-only ((command-handler command-handler))
    (with-gensyms (command
                   thread
                   ;; (2)
                   swank-emacs-connection
                   swank-send-counter)
      `(let* ((,command *command*)
              ;; (3)
              (,swank-emacs-connection swank::*emacs-connection*)
              (,swank-send-counter swank::*send-counter*)
              (,thread
                (bt:make-thread
                 #'(lambda ()
                     (let ((*command* ,command)
                           ;; (4)
                           (swank::*emacs-connection* ,swank-emacs-connection)
                           (swank::*send-counter* ,swank-send-counter))
                       ,@body))
                 :name "breeze command handler"
                 ;; :initial-bindings `((*package* ,*package*) ,@bt:*default-special-bindings*)
                 )))
         (setf (thread ,command-handler) ,thread)
         ,command-handler))))


(defun check-special-variables ()
  (unless *command*
    (error "In start-command: *command* is null"))
  (unless (context *command*)
    (error "In start-command: *command*'s context is null")))

(defun wait-for-sync (command)
  ;; Waiting for a message from start-command
  (log:debug "Waiting for 'sync message")
  (let ((sync-message (recv-into command)))
    (unless (eq 'sync sync-message)
      (error "In start-command: expected to receive the datum 'sync, received ~S instead"
             sync-message))
    (log:debug "Received 'sync message")))

(defun send-sync (command)
  ;; Send a message to the thread for synchronisation.
  (log:debug "Sending 'sync message...")
  (send-into command 'sync)
  (log:debug "'sync message sent"))

(defun wait-for-started-message (command)
  (log:debug "Waiting for the thread to be up and running...")
  ;; Wait for the thread to be up and running
  (let ((started-message (recv-from command)))
    (unless (eq 'started started-message)
      (error "In start-command: expected to receive the datum 'started, received ~S instead"
             started-message))))

(defun send-started-message (command)
  ;; Sending back a message to tell start-command the command is going to do its job
  (send-out command 'started))

(defun start-command (context-plist thunk)
  "Start processing a command, initializing *command*."
  ;; It is possible, especially (when testing breeze...) that there
  ;; are multiple commands running at the same time. For now, we'll
  ;; just cancel the currently running one.
  (cancel-command "start-command cleanup")
  (cancel-command-on-error
    (let ((command (make-instance
                    'command-handler
                    :context (context-plist-to-hash-table context-plist))))
      ;; Create the command handler with the right context
      (setf *command* command)
      ;; Create the thread for the command handler
      (make-command-thread (command)
        (cancel-command-on-error
          ;; Being very defensive here, because so many
          ;; things can go wrong.
          (check-special-variables)
          (wait-for-sync command)
          (send-started-message command)
          (funcall thunk)))
      (send-sync command)
      (wait-for-started-message command)
      (log:debug "Command started.")
      ;; Get the thread's request and return it.
      (run-command command nil))))


;; TODO Maybe rename ARGUMENTS to RESPONSE?
(defun continue-command (&rest arguments)
  "Continue procressing *command*."
  (let ((command *command*))
    (unless command
      (error "Continue-command called when no commands are currently running."))
    (cancel-command-on-error
      (run-command command arguments))))


;;; Utilities to get common stuff from the context

(defun context* ()
  "Get the *command*'s context."
  (if (command*)
      (context (command*))
      (warn "(command*) returned nil")))

(defun context-get (context key)
  "Get the value of KEY CONTEXT."
  (gethash key context))

(defun context-set (context key value)
  "Set KEY to VALUE in CONTEXT."
  (setf (gethash key context) value))

(defun context-buffer-string (context)
  "Get the \"buffer-string\" from the CONTEXT.
The buffer-string is the content of the buffer.
It can be null."
  (context-get context 'buffer-string))

(defun context-buffer-string* ()
  "Get the \"buffer-string\" from the *command*'s context.
The buffer-string is the content of the buffer.
It can be null."
  (context-get (context*) 'buffer-string))

(defun context-buffer-name (context)
  "Get the \"buffer-name\" from the CONTEXT.
The buffer-name is the name of the buffer.
It can be null."
  (context-get context 'buffer-name))

(defun context-buffer-name* ()
  "Get the \"buffer-name\" from the *command*'s context.
The buffer-name is the name of the buffer.
It can be null."
  (context-get (context*) 'buffer-name))

(defun context-buffer-file-name (context)
  "Get the \"buffer-file-name\" the CONTEXT.
The buffer-file-name is the name of the file that the buffer is
visiting.
It can be null."
  (context-get context 'buffer-file-name))

(defun context-buffer-file-name* ()
  "Get the \"buffer-file-name\" from the *command*'s context.
The buffer-file-name is the name of the file that the buffer is
visiting.
It can be null."
  (context-get (context*) 'buffer-file-name))

(defun context-point (context)
  "Get the \"point\" from the CONTEXT.
The point is the position of the cursor.
It can be null."
  (context-get context 'point))

(defun context-point* ()
  "Get the \"point\" from the *command*'s context.
The point is the position of the cursor.
It can be null."
  (context-get (context*) 'point))

(defun context-point-min (context)
  "Get the \"point-min\" from the CONTEXT.
The point-min is the position of the beginning of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (context-get context 'point-min))

(defun context-point-min* ()
  "Get the \"point-min\" from the *command*'s context.
The point-min is the position of the beginning of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (context-get (context*) 'point-min))

(defun context-point-max (context)
  "Get the \"point-max\" from the CONTEXT.
The point-max is the position of the end of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (context-get context 'point-max))

(defun context-point-max* ()
  "Get the \"point-max\" from the *command*'s context.
The point-max is the position of the end of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (context-get (context*) 'point-max))


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

(defun read-string-then-insert (prompt control-string)
  (insert control-string
          (read-string prompt)))

(defun choose (prompt collection)
  "Send a message to the editor to ask the user to choose one element
in the collection. The user can also enter a value not in the
collection."
  (send "choose" prompt collection)
  (recv1))

;; TODO This could benefit from FORMAT
(defun insert-at (position string save-excursion-p)
  "Send a message to the editor telling it to insert STRING at
POSITION. Set SAVE-EXCURSION-P to non-nil to keep the current
position."
  (send
   (if save-excursion-p
       "insert-at-saving-excursion"
       "insert-at")
   position
   string))

(defun replace-region (position-from position-to
                       replacement-string
                       &optional (save-excursion-p t))
  "Send a message to the editor telling it to replace the text between
POSITION-FROM POSITION-TO by REPLACEMENT-STRING. Set SAVE-EXCURSION-P
to non-nil to keep the current position."
  (send
   "replace"
   position-from
   position-to
   replacement-string
   save-excursion-p))

(defun backward-char (&optional n)
  "Send a message to the editor to move backward."
  (send "backward-char" n))

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


;;; Utilities to help creating commands less painful.

;; TODO This needs to be split-up!
(defmacro define-command (name (&rest key-arguments)
                          &body body)
  "Macro to define command with the basic context.
More special-purpose context can be passed, but it must be done so
using keyword arguments.

Example:

(define-command hi ()
  (message \"Hi ~a\" (read-string \"What is your name?\")))
"
  (let ((context-plist (symbolicate 'context-plist))
        (basic-context-variables (mapcar #'symbolicate
                                         '(buffer-string
                                           buffer-name
                                           buffer-file-name
                                           point
                                           point-min
                                           point-max))))
    (multiple-value-bind (remaining-forms declarations docstring)
        (alexandria:parse-body body :documentation t)
      ;; Docstring are mandatory for commands
      (check-type docstring string)
      `(defun ,name (&rest ,context-plist
                     &key
                       ,@basic-context-variables
                       ,@key-arguments)
         (declare (ignorable
                   ,context-plist
                   ,@basic-context-variables
                   ,@(loop for karg in key-arguments
                           collect (or (and (symbolp karg) karg)
                                       (first karg)))))
         ;; Add the users' declarations
         ,@(when declarations (list declarations))
         ,docstring
         (if (command*)
             ;; If we're already running a command
             (call-with-command-signal-handler
              (lambda ()
                ,@(loop
                    :for var :in basic-context-variables
                    :collect `(unless ,var
                                (setf ,var (,(let ((*package* #.*package*))
                                               (symbolicate 'context- var '*))))))
                ,@remaining-forms
                ;; Send the "done" command
                "done"))
             ;; If we're starting a new command
             (start-command
              ,context-plist
              (lambda ()
                (call-with-command-signal-handler
                 (lambda ()
                   (progn ,@remaining-forms)
                   (send "done"))))))))))


;;; Utilities to get more context

(defun parse-buffer (context)
  (let* ((buffer-string (context-buffer-string context))
         (code (parse-string buffer-string)))
    (breeze.reader:forms code)
    #++
    (unless (breeze.reader:forms code)
      (signal (breeze.reader:parser-condition code)))))

;; TODO Add lots of error-handling...
(defun augment-context-by-parsing-the-buffer (context)
  (let ((nodes (parse-buffer context)))
    (if nodes
        (let* (;; Find the node "at point"
               (path (find-path-to-node (context-point context) nodes))
               ;; Find the top-level form "at point"
               (outer-node (caar path))
               ;; Find the innermost form "at point"
               (inner-node (car (lastcar path)))
               (inner-node-index (cdr (lastcar path)))
               ;; Find the innermost form's parent
               (parent-node (car (before-last path))))
          #. `(progn ,@(loop :for key in '(nodes path outer-node
                                           inner-node inner-node-index parent-node)
                             :collect
                             `(context-set context ',key ,key)))
          t)
        nil)))


;;; Pieces of code to help debug issues with commands

;; (setf *break-on-signals* 'error)

;; (cancel-command)

#+ (or)
(trace
 start-command
 continue-command
 run-command
 call-with-cancel-command-on-error
 cancel-command
 donep)

;; (untrace)

#+ (or)
(trace
 insert*
 ;; start-command ; don't: too verbose
 cancel-command
 continue-command
 run-command
 donep

 %recv
 %send

 insert
 read-string
 read-string-then-insert
 choose)

#+ (or)
(trace
 chanl:send
 chanl:recv)

#+ (or)
(untrace)

;; (log:config :debug)
;; (log:config '(breeze command) :debug)
;; (log:config '(breeze command) :error)
