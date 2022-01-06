(in-package #:common-lisp-user)

(defpackage #:breeze.refactor
  (:use #:cl #:breeze.command)
  (:import-from
   #:breeze.utils
   #:symbol-package-qualified-name
   #:whitespacep)
  (:import-from
   #:breeze.reader
   #:parse-string
   ;; Accessors
   #:node-source
   #:node-start
   #:node-end
   #:node-content
   ;; Types of node
   #:skipped-node
   #:symbol-node
   #:read-eval-node
   #:character-node
   #:list-node
   #:function-node
   ;; Type predicates
   #:skipped-node-p
   #:symbol-node-p
   #:read-eval-node-p
   #:character-node-p
   #:list-node-p
   #:function-node-p)
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

(defun node-first (node)
  "Return the first element from a NODE's content."
  (first (node-content node)))

(defun node-lastcar (node)
  "Return the last element from a NODE's content."
  (alexandria:lastcar (node-content node)))

(defun node-string-equal (string node)
  "Compare the content of a NODE to a STRING, case-insensitive."
  (string-equal string (node-content node)))

(defun node-length (node &optional (ignore-skipped-p t))
  "Returns the length of a NODE's content."
  (and (list-node-p node)
       (length
	(if ignore-skipped-p
	    (remove-if #'skipped-node-p (node-content node))
	    (node-content node)))))

(defun node-symbol= (symbol node)
  "Does NODE represent the symbol SYMBOL."
  (and (symbol-node-p node)
       (node-string-equal (symbol-name symbol)
			  node)))

(defun null-node-p (node)
  "Does NODE represent the symbol \"nil\"."
  (node-symbol= 'nil node))


(defmacro define-node-form-predicates (types)
  `(progn
     ,@(loop :for type :in types
	     :collect
	     `(export
	       (defun ,(alexandria:symbolicate type '-form-p)
		   (node)
		 ,(format nil "Does NODE represent an \"~a\" form." type)
		 (and (list-node-p node)
		      (node-symbol= ',type (node-first node))))))))

(define-node-form-predicates
    (if
     defpackage
     in-package
     defparameter
     defvar
     loop))

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

(defun find-node (position nodes)
  (find-if #'(lambda (node)
	       (destructuring-bind (start . end)
		   (node-source node)
		 (and
		  (<= start position end)
		  (<= position end))))
	   nodes))

(defun find-path-to-node (position nodes)
  (loop :for node = (find-node position nodes)
	  :then (and (listp (node-content node))
		     (car (node-content node))
		     (find-node position (node-content node)))
	;; guard against infinite loop
	:repeat 100
	:while node
	;; :do (format t "~%~%~s" node)
	:collect node))

(defun find-nearest-sibling-form (nodes current-node predicate)
  (loop :with result
	:for node :in nodes
	:when (eq node current-node)
	  :do (return result)
	:when (funcall predicate node)
	  :do (setf result node)))

(defun find-nearest-in-package (nodes top-level-node)
  (find-nearest-sibling-form
   nodes top-level-node
   #'(lambda (node)
       )))

(defparameter *qf* nil
  "Data from the latest quickfix invocation.
For debugging purposes ONLY.")

#+ (or)
(let* ((*standard-output* *debug-io*)
       (pos (1- (getf *qf* :point)))
       (nodes (getf *qf* :nodes))
       (path (find-path-to-node pos nodes))
       (outer-node (car path))
       (inner-node (alexandria:lastcar path)))
  (loop :for node :in path
	:do (format t "~%===~%~s" node))
  (format t "~%~d-~d"
	  (node-start inner-node)
	  (node-end inner-node))
  (format t "~%~a"
	  (breeze.reader:unparse-to-string inner-node)))

#+(or)
(in-package-form-p
 (car (getf *qf* :nodes)))

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
	 (current-top-level-node (find-node (1- point) nodes))
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
    ;; Deduplicate commands
    (setf commands (remove-duplicates commands :key #'car))
    ;; Save some information for debugging
    (setf *qf* `(,@all
		 :nodes ,nodes
		 :current-top-level-node ,current-top-level-node
		 :commands ,commands))
    ;; Ask the user to choose a command
    (start-command
     all
     (choose "Choose a command: "
	     (mapcar #'second commands)
	     (lambda (choice)
	       (list "run-command"
		     (symbol-package-qualified-name
		      (car (find choice commands
				 :key #'second
				 :test #'string=)))))))))

#+nil
(quickfix :buffer-string "   " :point 3)
