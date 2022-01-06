(in-package #:common-lisp-user)

(defpackage #:breeze.refactor
  (:use #:cl #:breeze.command)
  (:import-from
   #:breeze.utils
   #:whitespacep)
  (:import-from
   #:breeze.reader
   #:parse-string
   #:node-source
   #:node-start
   #:node-end)
  (:export
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
   #:insert-defpackage
   #:insert-in-package-cl-user
   #:insert-asdf
   ;; Other commands
   #:quickfix))

(in-package #:breeze.refactor)


;;; Utility function

;; WIP
(defun breeze-buffer-has-defpackage ()
  "Check if a buffer already contains a defpackage form.")

;; WIP
(defun breeze-in-loop ()
  "Check if it's a valid place to add a loop clause.")

;; TODO A "skipped node" can also be a form hidden behing a feature (#+/-)
(defun emptyp (nodes)
  "Whether a list of node contains no code."
  (every #'(lambda (node)
	     (typep node 'breeze.reader:skipped-node))
	 nodes))


(defun defpackage-node-p (node)
  (and
   (typep node 'list-node)
   (let ((car (car (node-content node))))
     (and (typep car 'symbol-node)
	  (string-equal "defpackage" (node-content car))))))

;; TODO symbol-qualified-p : is the symbol "package-qualified"



;;; Insertion commands


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

(defun insert-defpackage (&rest all)
  "Insert a defpackage form."
  (start-command
   all
   (read-string "Name of the package: "
		(lambda (package-name)
		  (insert (list
			   "(cl:in-package #:common-lisp-user)~%~%~
                            (defpackage #:~a~%  (:use #:cl))~%~
                            ~%(in-package #:~a)"
			   package-name package-name))))))

(define-simple-command insert-in-package-cl-user
    "Insert (in-package #:cl-user)"
  (insert "(in-package #:cl-user)"))

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
			 ":depends-on '()"
			 ":serial t"
			 "  :components
    ((:module \"src\"
      :components ())
     (:module \"tests\"
      :components ())))"))))))))))))


;;; Quickfix

(defgeneric describe-command (command)
  (:documentation "Give a user-friendly description for a command.")
  (:method ((command symbol))
    (symbol-package-qualified-name command))
  (:method ((command (eql 'insert-defpackage)))
    "Insert a defpackage form."))

(defun command-description (function)
  (let ((doc (documentation function 'function)))
    (unless doc
      (error
       "Function ~s does not have a documentation string.~
                  Is it defined?"
       function))
    (list function doc)))

(defparameter *commands-applicable-at-toplevel*
  ;; Add insert-package
  (mapcar #'command-description
	  '(insert-asdf
	    insert-defun
	    insert-defmacro
	    insert-defpackage
	    insert-in-package-cl-user)))

(defparameter *commands-applicable-in-a-loop-form*
  (mapcar #'command-description
	  '(insert-loop-clause-for-in-list
	    insert-loop-clause-for-on-list
	    insert-loop-clause-for-hash)))

(defun find-top-level-node (position nodes)
  (find-if #'(lambda (node)
	       (destructuring-bind (start . end)
		   (node-source node)
		 (and
		  (<= start position end)
		  (<= position end))))
	   nodes))

(defun quickfix (&rest all
		 &key
		   buffer-string
		   buffer-name
		   buffer-file-name
		   point
		   point-min
		   point-max)
  (declare (ignorable all buffer-string buffer-name buffer-file-name
		      point point-min point-max))
  "Given the context, suggest some applicable commands."
  (let* (;; Parse the buffer
	 (nodes (parse-string buffer-string))
	 ;; Find the top-level form "at point"
	 (current-top-level-node (find-top-level-node (1- point)))
	 ;; Accumulate a list of commands that make sense to run in
	 ;; the current context
	 (commands))
    ;; When the buffer is empty, or only contains comments and
    ;; whitespaces.
    (when (emptyp nodes)
      (setf commands
	    (append *commands-applicable-at-toplevel* commands)))
    (when (or
	   ;; in-between forms
	   (null current-top-level-node)
	   ;; just at the start or end of a form
	   (= (1- point) (node-start current-top-level-node))
	   (= (1- point) (node-end current-top-level-node))
	   ;; inside a comment (or a form disabled by a
	   ;; feature-expression)
	   (typep current-top-level-node
		  'breeze.reader:skipped-node))
      (setf commands
	    (append *commands-applicable-at-toplevel* commands)))
    ;; Print debug information
    (when t
      (when current-top-level-node
	(format *debug-io* "~&Current top-level node's raw: ~s"
		(breeze.reader:node-raw current-top-level-node)))
      #+ (or)
      (format *debug-io* "~&Current node: ~a"
	      (breeze.reader:unparse-to-string current-top-level-node))
      ;; (format *debug-io* "~&Is current node a defpackage ~a" (defpackage-node-p current-top-level-node))
      (format *debug-io* "~&point ~a" point)
      (format *debug-io* "~&positions ~a" (mapcar #'node-source nodes))
      ;; (format *debug-io* "~&nodes ~a" nodes)
      )
    (setf commands (remove-duplicates commands :key #'car))
    (start-command
     all
     (choose "Choose a command: "
	     (mapcar #'second commands)
	     (lambda (choice)
	       (list "run-command"
		     (let ((*print-escape* t)
			   (*package* (find-package "KEYWORD"))
			   (function (car (find choice commands
						:key #'second
						:test #'string=))))
		       (prin1-to-string function)
		       #+nil
		       (format nil "(~s)" function))))))))

#+nil
(quickfix :buffer-string "   " :point 3)
