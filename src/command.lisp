(cl:in-package #:common-lisp-user)

(defpackage #:breeze.command
  (:use :cl)
  (:import-from #:alexandria
		#:plist-hash-table
		#:assoc-value
		#:symbolicate
		#:with-gensyms)
  (:export
   #:start-command
   #:call-next-callback
   ;; Functions to access the context
   #:command-context*
   #:context-buffer-string*
   #:context-buffer-name*
   #:context-buffer-file-name*
   #:context-point*
   #:context-point-min*
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

(defclass command-state ()
  ((tasklet
    :initform nil
    :initarg :tasklet
    :accessor command-tasklet
    :documentation
    "The tasklet that process the command.")
   (context
    :initarg :context
    :accessor command-context)))

(defmethod command-channel-in ((command-state command-state))
  (tasklet-channel-in
   (command-tasklet command-state)))

(defmethod command-channel-out ((command-state command-state))
  (tasklet-channel-out
   (command-tasklet command-state)))



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
	       :initial-bindings (acons '*current-command*
					(list *current-command*)
					chanl:*default-special-bindings*))))
       (change-class ,task 'tasklet)
       (setf (tasklet-channel-in ,task) ,channel-in
	     (tasklet-channel-out ,task) ,channel-out)
       ,task)))

#+ (or)
(make-tasklet ()
  (format t "~&hi!"))

(defun donep (command)
  (and (chanl:recv-blocks-p (command-channel-out command))
       (not (chanl:task-thread (command-tasklet command)))))

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

(defun run-callback (command arguments)
  "Receive request from the command tasklet, process them..."
  (if (donep command)
      (setf *current-command* nil)
      (progn
	(when arguments
	  (command-send command arguments))
	(command-recv command))))

(defun start-command (context-plist thunk)
  "Start processing a command, initializing *current-command*."
  ;; TODO We should (setf *current-command* nil) when there's an error
  (setf *current-command* (make-instance
			   'command-state
			   :context (plist-hash-table context-plist)))
  (setf (command-tasklet *current-command*)
	(make-tasklet ()
	  (funcall thunk)))
  (run-callback *current-command* nil))

(defun call-next-callback (&rest arguments)
  ;; TODO We should (setf *current-command* nil) when there's an error
  "Continue procressing *current-command*."
  (run-callback *current-command* arguments))

;; (call-next-callback)


;;; Utilities to get common stuff from the context

(defun command-context* ()
  "Get the *current-command*'s context."
  (when *current-command*
    (command-context *current-command*)))

(defun context-buffer-string* ()
  "Get the \"buffer-string\" from the *current-command*'s context.
The buffer-string is the content of the buffer.
It can be null."
  (gethash :buffer-string (command-context*)))

(defun context-buffer-name* ()
  "Get the \"buffer-name\" from the *current-command*'s context.
The buffer-name is the name of the buffer.
It can be null."
  (gethash :buffer-name (command-context*)))

(defun context-buffer-file-name* ()
  "Get the \"buffer-file-name\" from the *current-command*'s context.
The buffer-file-name is the name of the file that the buffer is
visiting.
It can be null."
  (gethash :buffer-string (command-context*)))

(defun context-point* ()
  "Get the \"point\" from the *current-command*'s context.
The point is the position of the cursor.
It can be null."
  (gethash :point (command-context*)))

(defun context-point-min* ()
  "Get the \"point-min\" from the *current-command*'s context.
The point-min is the position of the beggining of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (gethash :point-min (command-context*)))

(defun context-point-max* ()
  "Get the \"point-max\" from the *current-command*'s context.
The point-max is the position of the end of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (gethash :point-max (command-context*)))



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

(defun read-string (prompt)
  "Send a message to the editor to ask the user to enter a string."
  (send "read-string" prompt))

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
It is not necessary, but it makes it possible to close over the
arguments, which is nice."
  (let ((context (symbolicate 'context))
	(buffer-string (symbolicate 'buffer-string))
	(buffer-name (symbolicate 'buffer-name))
	(buffer-file-name (symbolicate 'buffer-file-name))
	(point (symbolicate 'point))
	(point-min (symbolicate 'point-min))
	(point-max (symbolicate 'point-max)))
    (multiple-value-bind (remaining-forms declarations docstring)
	(alexandria:parse-body body)
      `(defun ,name (&rest ,context
		     &key
		       (,buffer-string (context-buffer-string*))
		       (,buffer-name (context-buffer-name*))
		       (,buffer-file-name (context-buffer-file-name*))
		       (,point (context-point*))
		       (,point-min (context-point-max*))
		       (,point-max (context-point-max*))
		       ,@key-arguments)
	 (declare (ignorable
		   ,context
		   ,buffer-string
		   ,buffer-name
		   ,buffer-file-name
		   ,point
		   ,point-min
		   ,point-max
		   ,@(loop for karg in key-arguments
			   collect (or (and (symbolp karg) karg)
				       (first karg)))))
	 ,declarations
	 ,docstring
	 (if *current-command*
	     (progn ,@remaining-forms)
	     (start-command
	      ,context
	      (lambda ()
		,@remaining-forms)))))))

#+ (or)
(trace
 insert*
 ;; start-command ; don't: too verbose
 call-next-callback
 run-callback
 command-recv
 command-send
 donep

 %recv
 %send

 insert
 read-string
 read-string-then-insert)
