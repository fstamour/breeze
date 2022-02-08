(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.command
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
   ;; Utilities to create commands
   #:define-command
   #:handle))

(in-package #:breeze.command)

(defclass tasklet (chanl:task)
  ((channel-in
    :initform nil
    :accessor tasklet-channel-in
    :type (or null chanl:channel))
   (channel-out
    :initform nil
    :accessor tasklet-channel-out
    :type (or null chanl:channel)))
  (:documentation "A task (thread) with a channel."))

(defclass command-handler ()
  ((tasklet
    :initform nil
    :initarg :tasklet
    :accessor command-tasklet
    :documentation
    "The tasklet that process the command.")
   (context
    :initarg :context
    :accessor command-context)))

(defmethod tasklet-channer-in ((_ (eql nil)))
  (warn "tasklet-channer-in called on nil.")
  nil)
(defmethod tasklet-channer-out ((_ (eql nil)))
  (warn "tasklet-channer-out called on nil.")
  nil)

(defmethod command-channel-in ((command-handler command-handler))
  (if-let ((tasklet (command-tasklet command-handler)))
    (tasklet-channel-in tasklet)))

(defmethod command-channel-out ((command-handler command-handler))
  (if-let ((tasklet (command-tasklet command-handler)))
    (tasklet-channel-out tasklet)))



;; No, I won't support multiple client/command at the same time, for
;; now™.
(defparameter *current-command* nil
  "The command that is currently being executed.")

(defparameter *channel-in* nil
  "The channel use to send data to the command's tasklet.")

(defparameter *channel-out* nil
  "The channel used to receive data from the command's tasklet.")

(defmacro make-tasklet (() &body body)
  "Creates a thread, return a channel to communicate with it."
  (with-gensyms (channel-in channel-out task)
    `(let* ((,channel-in (make-instance 'chanl:bounded-channel))
	    (,channel-out (make-instance 'chanl:bounded-channel))
	    (,task
	      (chanl:pcall
	       #'(lambda ()
		   (let ((*channel-in* ,channel-in)
			 (*channel-out* ,channel-out))
		     ,@body))
	       :name "breeze command"
	       :initial-bindings `((*package* ,*package*)
				   (*current-command* ,*current-command*)
				   ,@chanl:*default-special-bindings*))))
       (change-class ,task 'tasklet)
       (setf (tasklet-channel-in ,task) ,channel-in
	     (tasklet-channel-out ,task) ,channel-out)
       ,task)))

#+ (or)
(make-tasklet ()
  (format t "~&hi!"))

(defun cancel-command (&optional reason)
  ;; TODO Kill the associated thread.
  (setf *current-command* nil)
  (when reason
    ;; TODO We probably don't want that...
    (error reason))
  ;; (format *debug-io* "~&*current-command* reset.")
  )



;; TODO This function the festival of race conditions
(defun donep (command)
  (or
   (null command)
   (not (chanl:task-thread (command-tasklet command)))))

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

(defun command-recv (command)
  (let ((request (%recv (command-channel-out command))))
    (and (listp request) request)))

(defun command-send (command value)
  (%send (command-channel-in command) value))

(defun run-command (command arguments)
  "Receive request from the command tasklet, process them..."
  (if (donep command)
      (cancel-command)
      (progn
	(when arguments
	  (command-send command arguments))
	(let ((request
		(command-recv command)))
	  (unless request
	    (cancel-command "Request is null."))
	  request))))

(defun call-with-cancel-command-on-error (thunk)
  (handler-bind
      ((error #'(lambda (condition)
		  ;; (declare (ignore condition))
		  (format *debug-io* "~&An error occured: ~a" condition)
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

;; TODO unit test
#+ (or)
(alexandria:hash-table-plist
 (context-plist-to-hash-table '(buffer-string "asdf" ok 42)))
;; => (OK 42 :BUFFER-STRING "asdf")

;; (setf *break-on-signals* 'error)

(defun start-command (context-plist thunk)
  "Start processing a command, initializing *current-command*."
  (cancel-command-on-error
    ;; Create the command handler with the right context
    (setf *current-command*
	  (make-instance
	   'command-handler
	   :context (context-plist-to-hash-table context-plist)))
    ;; Create the thread for the command handler
    (setf (command-tasklet *current-command*)
	  (make-tasklet ()
	    (cancel-command-on-error
	      ;; Send a message to tell the thread is up and running
	      (chanl:send *channel-out* :started)
	      (funcall thunk))))
    ;; Wait for the thread to be up and running
    (chanl:recv
     (command-channel-out *current-command*))
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
  (when (current-command*)
    (command-context (current-command*))))

(defun context-get (context key)
  (gethash key context))

(defun context-set (context key value)
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



(defmacro handle (lambda-list &body body)
  "Wait for a message from *channel*, process it and return a the
result of the last form on *channel*."
  `(destructuring-bind ,lambda-list
       (%recv *channel-in*)
     ,@body))

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

#+ (or)
(define-command insert-test
    nil
  ""
  (start-command context
		 (lambda ()
                   (insert "Test"))))

(defun read-string (prompt &optional initial-input)
  "Send a message to the editor to ask the user to enter a string."
  (send "read-string" prompt initial-input))

(defun read-string-then-insert (prompt control-string)
  (read-string prompt)
  (handle (string)
    (insert control-string string)))

#+ (or)
(define-command read-string-then-insert-test
    nil
  ""
  (start-command
   context
   (lambda ()
     (read-string-then-insert "? " "-> ~a <-"))))



(defun choose (prompt collection)
  "Send a message to the editor to ask the user to choose one element
in the collection."
  (send "choose" prompt collection))

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
		       save-excursion-p)
  "Send a message to the editor telling it to replace the text between
POSITION-FROM POSITION-TO by REPLACEMENT-STRING. Set SAVE-EXCURSION-P
to non-nil to keep the current position."
  (send
   (if save-excursion-p
       "replace-saving-excursion"
       "replace")
   position-from
   position-to
   replacement-string))

(defun backward-char (&optional n)
  (send "backward-char" n))


;;; Utilities to help creating commands less painful.

(defmacro define-command (name (&rest key-arguments)
			  &body body)
  "Macro to define command with the basic context.
More special-purpose context can be passed, but it must be done so
using keyword arguments."
  (let ((context (symbolicate 'context))
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
      `(defun ,name (&rest ,context
		     &key
		       ,@basic-context-variables
		       ,@key-arguments)
	 (declare (ignorable
		   ,context
		   ,@basic-context-variables
		   ,@(loop for karg in key-arguments
			   collect (or (and (symbolp karg) karg)
				       (first karg)))))
	 ,@(when declarations (list declarations))
	 ,docstring
	 (if (current-command*)
	     (progn
	       ,@(loop :for var :in basic-context-variables
		       :collect `(unless ,var
				   (setf ,var (,(symbolicate 'context- var '*)))))
	       ,@remaining-forms)
	     (start-command
	      ,context
	      (lambda ()
		,@remaining-forms)))))))

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
 command-recv
 command-send
 donep

 %recv
 %send

 insert
 read-string
 read-string-then-insert
 choose)
