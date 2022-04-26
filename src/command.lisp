(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.command
  (:documentation "Interactive commands' core")
  (:use :cl)
  (:import-from #:alexandria
                #:symbolicate
                #:with-gensyms
                #:if-let)
  (:export
   #:start-command
   #:cancel-command
   #:continue-command
   ;; Functions to access the context
   #:command-context
   #:command-context*
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
   ;; Utilities to create commands
   #:define-command))

(in-package #:breeze.command)

(defclass command-handler ()
  ((thread
    :initform nil
    :initarg :thread
    :accessor command-thread
    :documentation "The thread that process the command.")
   (channel-in
    :initform (make-instance 'chanl:bounded-channel)
    :accessor command-channel-in
    :type (or null chanl:channel)
    :documentation "The channel to send response to the thread.")
   (channel-out
    :initform (make-instance 'chanl:bounded-channel)
    :accessor command-channel-out
    :type (or null chanl:channel)
    :documentation "The channel to receive requests from the thread.")
   (context
    :initarg :context
    :initform nil
    :accessor command-context
    :documentation "The context of the command."))
  (:documentation "Contains the runtime data to run a command."))

(defmethod command-channel-in :around (command)
  "Helper method for easier debugging."
  (let ((result (call-next-method)))
    (unless result
      (error "(command-channel-in ~s) returned nil" command))
    result))

(defmethod command-channel-out :around (command)
  "Helper method for easier debugging."
  (let ((result (call-next-method)))
    (unless result
      (error "(command-channel-out ~s) returned nil" command))
    result))

(defmethod command-channel-in ((_ (eql nil)))
  "Helper method for easier debugging."
  (declare (ignore _))
  (warn "command-channel-in called on nil.")
  nil)

(defmethod command-channel-out ((_ (eql nil)))
  "Helper method for easier debugging."
  (declare (ignore _))
  (warn "command-channel-out called on nil.")
  nil)



;; No, I won't support multiple client/command at the same time, for
;; now™.
(defparameter *current-command* nil
  "The command that is currently being executed.")

(defparameter *channel-in* nil
  "The channel use to send data to the command's thread.")

(defparameter *channel-out* nil
  "The channel used to receive data from the command's thread.")


(defun cancel-command (&optional reason)
  "Cancel the currently running command."
  (if-let ((thread (and
                    *current-command*
                    (command-thread *current-command*))))
    (unless (or (eq (bt:current-thread) thread)
                (not (bordeaux-threads:thread-alive-p thread)))
      ;; Kill the associated thread.
      (bt:destroy-thread thread)))
  ;; Clear the special variable
  (setf *current-command* nil)
  ;; Log
  (if reason
      (log:debug "Command canceled because ~a." reason)
      (log:debug "Command canceled for unspecified reason."))
  ;; Return nil, because it feels like the right thing to do
  nil)

(defun donep (command)
  (null command))

(defun current-command* ()
  "Helper to get the current command, or correctly set it to nil."
  (when *current-command*
    (if (donep *current-command*)
        (cancel-command "Done")
        *current-command*)))

(defun %recv (channel)
  "To help with tracing."
  (chanl:recv channel))

(defun %send (channel value)
  "To help with tracing."
  (chanl:send channel value))

(defun run-command (command arguments)
  "Receive request from the command's thread."
  (if (donep command)
      (cancel-command)
      (progn
        (when arguments (chanl:send (command-channel-in command) arguments))
        (let ((request (chanl:recv (command-channel-out command))))
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
                  (log:error "~&An error occured: ~a" condition)
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
                              (intern (symbol-name key) :keyword)
                              key)
    :when value
      :do (setf (gethash normalized-key ht) value)
    :finally (return ht)))

(defmacro make-command-thread ((command-handler) &body body)
  "Initialize a command-handler's thread."
  (alexandria:once-only ((command-handler command-handler))
    (with-gensyms (channel-in channel-out thread)
      `(let* ((,channel-in (command-channel-in ,command-handler))
              (,channel-out (command-channel-out ,command-handler))
              (,thread
                (bt:make-thread
                 #'(lambda ()
                     (let ((*channel-in* ,channel-in)
                           (*channel-out* ,channel-out))
                       (declare (ignorable *channel-in*
                                           *channel-out*))
                       ,@body))
                 :name "breeze command handler"
                 ;; :initial-bindings `((*package* ,*package*) ,@bt:*default-special-bindings*)
                 )))
         (setf (command-thread ,command-handler) ,thread)
         ,command-handler))))

(defun start-command (context-plist thunk)
  "Start processing a command, initializing *current-command*."
  (cancel-command-on-error
    ;; Create the command handler with the right context
    (setf *current-command*
          (make-instance
           'command-handler
           :context (context-plist-to-hash-table context-plist)))
    ;; Create the thread for the command handler
    (make-command-thread (*current-command*)
      (cancel-command-on-error
        ;; Being very defensive here, because so many
        ;; things can go wrong.
        (unless *channel-in*
          (error "In start-command: *channel-in* is null"))
        (unless *channel-out*
          (error "In start-command: *channel-out* is null"))
        (unless *current-command*
          (error "In start-command: *current-command* is null"))
        (unless (command-context *current-command*)
          (error "In start-command: *current-command*'s context is null"))
        ;; Waiting for a message from start-command
        (let ((sync-message (chanl:recv *channel-in*)))
          (unless (eq :sync sync-message)
            (error "In start-command: expected to receive the datum :sync, received ~S instead"
                   sync-message)))
        ;; Sending back a message to tell start-command the command is going to do its job
        (chanl:send *channel-out* :started)
        (funcall thunk)))
    ;; Send a message to the thread for synchronisation.
    (chanl:send (command-channel-in *current-command*) :sync)
    (log:debug "Waiting for the thread to be up and running...")
    ;; Wait for the thread to be up and running
    (let ((started-message (chanl:recv (command-channel-out *current-command*))))
      (unless (eq :started started-message)
        (error "In start-command: expected to receive the datum :started, received ~S instead"
               started-message)))
    (log:debug "Command started.")
    ;; Get the thread's request and return it.
    (run-command *current-command* nil)))


(defun continue-command (&rest arguments)
  "Continue procressing *current-command*."
  (unless *current-command*
    (error "Continue-command called when no commands are currently running."))
  (cancel-command-on-error
    (run-command *current-command* arguments)))


;;; Utilities to get common stuff from the context

(defun command-context* ()
  "Get the *current-command*'s context."
  (if (current-command*)
      (command-context (current-command*))
      (warn "(current-command*) returned nil")))

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
  (context-get context :buffer-string))

(defun context-buffer-string* ()
  "Get the \"buffer-string\" from the *current-command*'s context.
The buffer-string is the content of the buffer.
It can be null."
  (context-get (command-context*) :buffer-string))

(defun context-buffer-name (context)
  "Get the \"buffer-name\" from the CONTEXT.
The buffer-name is the name of the buffer.
It can be null."
  (context-get context :buffer-name))

(defun context-buffer-name* ()
  "Get the \"buffer-name\" from the *current-command*'s context.
The buffer-name is the name of the buffer.
It can be null."
  (context-get (command-context*) :buffer-name))

(defun context-buffer-file-name (context)
  "Get the \"buffer-file-name\" the CONTEXT.
The buffer-file-name is the name of the file that the buffer is
visiting.
It can be null."
  (context-get context :buffer-file-name))

(defun context-buffer-file-name* ()
  "Get the \"buffer-file-name\" from the *current-command*'s context.
The buffer-file-name is the name of the file that the buffer is
visiting.
It can be null."
  (context-get (command-context*) :buffer-file-name))

(defun context-point (context)
  "Get the \"point\" from the CONTEXT.
The point is the position of the cursor.
It can be null."
  (context-get context :point))

(defun context-point* ()
  "Get the \"point\" from the *current-command*'s context.
The point is the position of the cursor.
It can be null."
  (context-get (command-context*) :point))

(defun context-point-min (context)
  "Get the \"point-min\" from the CONTEXT.
The point-min is the position of the beggining of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (context-get context :point-min))

(defun context-point-min* ()
  "Get the \"point-min\" from the *current-command*'s context.
The point-min is the position of the beggining of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (context-get (command-context*) :point-min))

(defun context-point-max (context)
  "Get the \"point-max\" from the CONTEXT.
The point-max is the position of the end of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (context-get context :point-max))

(defun context-point-max* ()
  "Get the \"point-max\" from the *current-command*'s context.
The point-max is the position of the end of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (context-get (command-context*) :point-max))




(defun recv ()
  "Returns a list"
  (%recv *channel-in*))

(defun recv1 ()
  (car (%recv *channel-in*)))

(defun send (request &rest data)
  (chanl:send *channel-out* `(,request ,@data)))


;;; Basic commands, to be composed

(defun insert (control-string &rest args)
  "Send a message to the editor telling it to insert STRING at
POSITION. Set SAVE-EXCURSION-P to non-nil to keep the current
position."
  (send
   "insert"
   (apply #'format nil control-string args)))

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
        (apply #'format nil control-string format-arguments)))

(defun find-file (pathname)
  "Send a message to the editor to tell it to open the PATHNAME."
  (send "find-file"
        (namestring pathname)))


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
         (if (current-command*)
             (progn
               (block nil
                 ,@(loop
                     :for var :in basic-context-variables
                     :collect `(unless ,var
                                 (setf ,var (,(let ((*package* #.*package*))
                                                (symbolicate 'context- var '*))))))
                 ,@remaining-forms)
               ;; Send the "done" command
               "done")
             (start-command
              ,context-plist
              (lambda ()
                (progn
                  (block nil ,@remaining-forms)
                  (send "done")))))))))

;; (setf *break-on-signals* 'error)

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
