(cl:in-package #:common-lisp-user)

(defpackage #:channel-for-commands
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:bt #:bordeaux-threads)
   (#:c #:chanl)))

(in-package #:channel-for-commands)


;;; I realized that chanl could be used instead of the annoying CPS
;;; (continuation passing style) stuff to implement the commands,
;;; let's try it!
;;;
;;; First thing, we need to create a channel and a thread, and use the
;;; channel.

(let ((channel (make-instance 'c:channel)))
  (bt:make-thread
   #'(lambda ()
       (let ((x (c:recv channel)))
	 (c:send channel (* 2 x))))
   :name "breeze command")
  (c:send channel 42)
  (c:recv channel))


;;; Right away, I can think of 2 little utilities that could help.

(defparameter *channel* nil
  "The channel used for interacting with commands.")

(defun call (&rest args)
  "Send ARGS to *channel* and wait for a return value."
  (c:send *channel* args)
  (c:recv *channel*))

(defmacro handle (lambda-list &body body)
  "Wait for a message from *channel*, process it and return a the
result of the last form on *channel*."
  `(destructuring-bind ,lambda-list
       (c:recv *channel*)
     (c:send *channel*
	     (progn
	       ,@body))))

;;; And we can use c:pcall as a drop-in replacement for br:make-thread
;;; it'll return a nicer object.

(let* ((*channel* (make-instance 'c:channel)))
  (c:pcall
   #'(lambda ()
       (handle (x)
	 (* 2 x)))
   :name "breeze command"
   :initial-bindings (acons '*channel*
			    (list *channel*)
			    c:*default-special-bindings*))
  (call 42))

;;; And yest another helper-macro

(defmacro tasklet (() &body body)
  "Creates a thread, return a channel to communicate with it."
  (a:with-gensyms (channel)
    `(let* ((,channel (make-instance 'c:channel)))
       (c:pcall
	#'(lambda ()
	    (let ((*channel* ,channel))
	      ,@body))
	:name "breeze command")
       ,channel)))

;; Now it seems much more user-friendly than CPS!
(let ((*channel* (tasklet ()
		   (handle (x)
		     (* 2 x)))))
  (call 42))
