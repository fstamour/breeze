(cl:in-package #:common-lisp-user)

(defpackage #:breeze.command
  (:use :cl)
  (:import-from #:alexandria
		#:plist-alist
		#:assoc-value)
  (:export
   #:start-command
   #:call-next-callback
   ;; Functions to access the context
   #:command-context*
   #:context-buffer-string
   #:context-buffer-name
   #:context-buffer-file-name
   #:context-point
   #:context-point-min
   #:context-point-max
   ;; Basic composables commands
   #:choose
   #:read-string
   #:insert-at
   #:insert
   #:read-string-then-insert
   #:replace-region
   #:backward-char
   ;; Utilities to create commands
   #:chain
   #:defcommand
   #:define-simple-command
   ;; Simple commands
   #:insert-loop-clause-for-on-list
   #:insert-loop-clause-for-in-list
   #:insert-loop-clause-for-hash
   #:insert-defvar
   #:insert-defparameter
   #:insert-defconstant
   #:insert-define-constant
   #:insert-defun-shaped
   #:insert-defun
   #:insert-defmacro
   #:insert-asdf))

(in-package #:breeze.command)

(defclass command-state ()
  ((callback
    :initarg :callback
    :accessor command-callback)
   (context
    :initarg :context
    :accessor command-context)))

;; No, I won't support multiple client/command at the same time, for
;; now.
(defparameter *current-command* nil
  "The command that is currently being executed.")

(defun run-callback (callback arguments)
  (multiple-value-bind (result-or-another-callback next-callback)
      (when callback
	(apply callback arguments))
    (if (functionp result-or-another-callback)
	;; A callback can return another functions, this is used to
	;; pass arguments to further callbacks by closing over the
	;; arguments.
	(run-callback result-or-another-callback nil)
	(progn
	  ;; Update *current-comand*'s callback
	  (cond	  ; using "cond" instead of "if" for future expansions
	    ;; Don't update, we'll loop on the current callback
	    ((eq t next-callback))
	    (t
	     (setf (command-callback *current-command*)
		   next-callback)))
	  ;; Return the result
	  result-or-another-callback))))

(defun start-command (context-alist first-callback)
  "Start processing a command, initializing *current-command*."
  (setf *current-command* (make-instance
			   'command-state
			   :callback first-callback
			   :context context-alist))
  (run-callback first-callback nil))

(defun call-next-callback (&rest arguments)
  "Continue procressing *current-command*."
  (run-callback (command-callback *current-command*) arguments))


;;; Utilities to get common stuff from the context

(defun command-context* ()
  "Get the *current-command*'s context."
  (command-context *current-command*))

(defun context-buffer-string ()
  "Get the \"buffer-string\" from the *current-command*'s context.
The buffer-string is the content of the buffer.
It can be null."
  (alexandria:assoc-value (command-context*) :buffer-string))

(defun context-buffer-name ()
  "Get the \"buffer-name\" from the *current-command*'s context.
The buffer-name is the name of the buffer.
It can be null."
  (alexandria:assoc-value (command-context*) :buffer-name))

(defun context-buffer-file-name ()
  "Get the \"buffer-file-name\" from the *current-command*'s context.
The buffer-file-name is the name of the file that the buffer is
visiting.
It can be null."
  (alexandria:assoc-value (command-context*) :buffer-string))

(defun context-point ()
  "Get the \"point\" from the *current-command*'s context.
The point is the position of the cursor.
It can be null."
  (alexandria:assoc-value (command-context*) :point))

(defun context-point-min ()
  "Get the \"point-min\" from the *current-command*'s context.
The point-min is the position of the beggining of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (alexandria:assoc-value (command-context*) :point))

(defun context-point-max ()
  "Get the \"point-max\" from the *current-command*'s context.
The point-max is the position of the end of buffer-string.
See \"narrowing\" in Emacs.
It can be null."
  (alexandria:assoc-value (command-context*) :point))


;;; Basic commands, to be composed

(defun choose (prompt collection &optional callback)
  "Send a message to the editor to ask the user to choose one element
in the collection."
  (lambda ()
    (values
     `("choose" ,prompt ,collection)
     callback)))

(defun read-string (prompt &optional callback)
  "Send a message to the editor to ask the user to enter a string."
  (lambda ()
    (values
     `("read-string" ,prompt)
     callback)))

(defun insert-at (position string save-excursion-p &optional callback)
  "Send a message to the editor telling it to insert STRING at
POSITION. Set SAVE-EXCURSION-P to non-nil to keep the current
position."
  (lambda ()
    (values (list
	     (if save-excursion-p
		 "insert-at-saving-excursion"
		 "insert-at")
	     position
	     string)
	    callback)))

(defun insert (string-spec &optional callback)
  "Send a message to the editor telling it to insert STRING at
POSITION. Set SAVE-EXCURSION-P to non-nil to keep the current
position."
  (lambda ()
    (values (list "insert"
		  (if (listp string-spec)
		      (apply #'format nil string-spec)
		      string-spec))
	    callback)))

(defun read-string-then-insert (prompt control-string
				&optional callback)
  (read-string prompt
	       (lambda (string)
		 (insert (list control-string string) callback))))

(defun replace-region (position-from position-to
		       replacement-string
		       save-excursion-p
		       &optional callback)
  "Send a message to the editor telling it to replace the text between
POSITION-FROM POSITION-TO by REPLACEMENT-STRING. Set SAVE-EXCURSION-P
to non-nil to keep the current position."
  (lambda ()
    (values (list
	     (if save-excursion-p
		 "replace-saving-excursion"
		 "replace")
	     position-from
	     position-to
	     replacement-string)
	    callback)))

(defun backward-char (&optional n callback)
  (lambda ()
    (values (list "backward-char" n)
	    callback)))


;;; Utilities to help creating commands less painful.

(defmacro defcommand (name (&rest key-arguments) &body body)
  "Macro to define command with the basic context.
It is not necessary, but it makes it possible to close over the
arguments, which is nice."
  `(defun ,name (&rest context
		 &key
		   buffer-string
		   buffer-name
		   buffer-file-name
		   point
		   point-min
		   point-max
		   ,@key-arguments)
     (declare (ignorable buffer-string
			 buffer-name
			 buffer-file-name
			 point
			 point-min
			 point-max
			 ,@(loop for karg in key-arguments
				 collect (or (and (symbolp karg) karg)
					     (first karg)))))
     ,@body))

(defmacro chain (&body forms)
  (reduce (lambda (acc next)
	    (append acc (list next)))
	  (butlast forms)
	  :initial-value (alexandria:lastcar forms)
	  :from-end t))

(defmacro chain* (&body forms)
  (alexandria:with-gensyms (callback)
    `(lambda (,callback)
       (chain ,@forms ,callback))))

(defmacro define-simple-command (name docstring &body commands)
  "Macro to define simple commands consisting of a chain of composable
commands."
  (check-type docstring string)
  `(defcommand ,name ()
     ,docstring
     (start-command
      context
      (chain
	,@commands))))


;;; Define some simple commands (snippets).

(define-simple-command insert-loop-clause-for-on-list
    "Insert a loop clause to iterate on a list."
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :on ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(define-simple-command insert-loop-clause-for-in-list
    "Insert a loop clause to iterate in a list."
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :in ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(define-simple-command insert-loop-clause-for-hash
    "Insert a loop clause to iterate on a hash-table."
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the key: "
   "~a :being :the :hash-key :of ")
  (read-string-then-insert
   "Enter the variable name for the hash-table: "
   "~a :using (hash-value ")
  (read-string-then-insert
   "Enter the variable name for the value: "
   "~a)"))

(defun insert-defvar-shaped (form-name all)
  "Start a command to insert a form that has the same shape as a
defvar."
  (start-command
   all
   (chain
     (insert (list "(~a " form-name))
     ;; TODO Check if name is surrounded by "*"
     (read-string-then-insert "Name: " "*~a* ")
     (read-string-then-insert "Initial value: " "~a~%")
     (read-string-then-insert "Documentation string " "\"~a\")"))))

(defun insert-defvar (&rest all)
  "Insert a defvar form."
  (insert-defvar-shaped "defvar" all))

(defun insert-defparameter (&rest all)
  "Insert a defparameter form."
  (insert-defvar-shaped "defparameter" all))

(defun insert-defconstant (&rest all)
  "Insert a defconstant form."
  (insert-defvar-shaped "defconstant" all))

;; TODO Add "alexandria" when the symbol is not interned
;;      ^^^ that should go in "refactor.lisp"
(defun insert-define-constant (&rest all)
  "Insert a alexandria:define-constant form."
  (insert-defvar-shaped "define-constant" all))


(defun insert-defun-shaped (form-name all)
  "Start a command to insert a form that has the same shape as a
defun."
  (start-command
   all
   (chain
     (insert (list "(~a " form-name))
     (read-string-then-insert "Name: " "~a (")
     (read-string-then-insert
      ;; Who needs to loop...?
      "Enter the arguments: " "~a)~%)")
     (backward-char))))

(defun insert-defun (&rest all)
  "Insert a defun form."
  (insert-defun-shaped "defun" all))

(defun insert-defmacro (&rest all)
  "Insert a defmacro form."
  (insert-defun-shaped "defmacro" all))

;; TODO insert-let (need to loop probably)

(defcommand insert-asdf ()
  "Insert an asdf system definition."
  (start-command
   context
   (read-string
    "Name of the system: "
    (lambda (system-name)
      (read-string
       "Author: "
       (lambda (author)
	 (read-string
	  "Licence name: "
	  (lambda (licence)
	    (chain
	      (insert '("(cl:in-package #:cl)~%~%"))
	      (insert (list
		       "(defpackage #:~a.asd~%  (:use :cl :asdf))~%~%"
		       system-name))
	      (insert (list
		       "(in-package #:~a.asd)~%~%"
		       system-name))
	      (insert (list
		       "(defsystem \"~a\"~{  ~a~%~}"
		       system-name
		       `(":description \"\""
			 ":version \"0.0.1\""
			 ,(format nil ":author \"~a\"" author)
			 ,(format nil ":licence \"~a\"" licence)
			 ":serial t"
			 "  :components
    ((:module \"src\"
      :components ())
     (:module \"tests\"
      :components ())))"))))))))))))
