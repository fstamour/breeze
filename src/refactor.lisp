(in-package #:common-lisp-user)

(defpackage #:breeze.refactor
  (:use #:cl #:breeze.command)
  (:import-from
   #:breeze.utils
   #:symbol-package-qualified-name
   #:before-last)
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



(defun find-node (position nodes)
  (loop :for node :in nodes
	:for (start . end) = (node-source node)
	:for i :from 0
	:when (and
	       (<= start position end)
	       (<= position end))
	  :do
	     (return (cons node i))))

(defun find-path-to-node (position nodes)
  (loop :for found = (find-node position nodes)
	  :then (let ((node (car found)))
		  (and (listp (node-content node))
		       (car (node-content node))
		       (find-node position (node-content node))))
	:while found
	:collect found))

(defun find-nearest-sibling-form (nodes current-node predicate)
  "Find the nearest sibling form that match the predicate."
  (loop :with result
	:for node :in nodes
	:when (eq node current-node)
	  :do (return result)
	:when (funcall predicate node)
	  :do (setf result node)))

(defmacro define-find-nearest-sibling-form (types)
  `(progn
     ,@(loop :for type :in types
	     :collect
	     `(export
	       (defun ,(alexandria:symbolicate
			'find-nearest-sibling- type '-form)
		   (nodes current-node)
		 ,(format
		   nil "Find the nearest sibling form of type \"~a\"."
		   type)
		 (find-nearest-sibling-form
		  nodes current-node
		  #',(alexandria:symbolicate type '-form-p)))))))

(defun find-nearest-parent-form (path predicate)
  "Find the nearest parent form that match the predicate."
  (loop :with result
	:for node :in path
	:when (funcall predicate node)
	  :do (setf result node)
	:finally (return result)))

(defmacro define-find-nearest-parent-form (types)
  `(progn
     ,@(loop :for type :in types
	     :collect
	     `(export
	       (defun ,(alexandria:symbolicate
			'find-nearest-parent- type '-form)
		   (path)
		 ,(format
		   nil "Find the nearest parent form of type \"~a\"."
		   type)
		 (find-nearest-parent-form
		  path
		  #',(alexandria:symbolicate type '-form-p)))))))



(defmacro define-node-utilities (types)
  `(progn
     (define-node-form-predicates ,types)
     (define-find-nearest-sibling-form ,types)
     (define-find-nearest-parent-form ,types)))

(define-node-utilities
    (if
     when unless
     defpackage
     in-package
     defparameter
     defvar
     loop
     defun
     defmacro
     defmethod
     defgeneric
     defconstant
     defclass
     let
     flet
     labels
     lambda))

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
    "Insert (cl:in-package #:cl-user)"
  (insert "(cl:in-package #:cl-user)"))

;; TODO insert-let (need to loop probably)

(defcommand insert-asdf ()
  "Insert an asdf system definition form."
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
	      ;; TODO don't insert a defpackage if it already exists
	      (insert (list
		       "(defpackage #:~a.asd~%  (:use :cl :asdf))~%~%"
		       system-name))
	      (insert (list
		       "(in-package #:~a.asd)~%~%"
		       system-name))
	      (insert (list
		       "(asdf:defsystem #:~a~%~{  ~a~%~}"
		       system-name
		       `(":description \"\""
			 ":version \"0.0.1\""
			 ,(format nil ":author \"~a\"" author)
			 ,(format nil ":licence \"~a\"" licence)
			 ":depends-on '()"
			 ";; :pathname src"
			 ":serial t"
			 "  :components
    (#+(or) (:file \"todo.lisp\")))"))))))))))))

;; TODO How could I re-use an hypothetical "insert-slot" command?
(defcommand insert-defclass ()
  "Insert a defclass form."
  (start-command
   context
   (read-string-then-insert
    "Name of the class: "
    "(defclass ~a ()~
   ~%  ((slot~
   ~%    :initform nil~
   ~%    :initarg :slot~
   ~%    :accessor ~@*~a-slot))~
   ~%  (:documentation \"\"))")))

(defcommand insert-defgeneric ()
  "Insert a defgeneric form."
  (start-command
   context
   (read-string-then-insert
    "Name of the generic: "
    "(defgeneric ~a ()~
   ~%  (:documentation \"\")~
   ~%  (:method () ()))")))

(defcommand insert-print-unreadable-object-boilerplate ()
  "Insert a print-object method form."
  (start-command
   context
   (read-string
    "Name of the object (paramater name of the method): "
    (lambda (name)
      (read-string
       "Type of the object: "
       (lambda (type)
	 (insert
	  (list
	   "(defmethod print-object ((~a ~a) stream)~
          ~%  (print-unreadable-object~
          ~%      (~a stream :type t :identity nil)~
          ~%    (format stream \"~~s\" (~a-something ~a))))"
	   name type
	   name
	   type name))))))))

;; TODO quick-insert (format *debug-io* "~&")


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
	    insert-in-package-cl-user
	    insert-defvar
	    insert-defparameter
	    insert-defclass
	    insert-defgeneric
	    insert-print-unreadable-object-boilerplate)))

(defparameter *commands-applicable-in-a-loop-form*
  (mapcar #'command-description
	  '(insert-loop-clause-for-in-list
	    insert-loop-clause-for-on-list
	    insert-loop-clause-for-hash)))


(defparameter *qf* nil
  "Data from the latest quickfix invocation.
For debugging purposes ONLY.")

#+ (or)
(let* ((*standard-output* *debug-io*)
       (pos (1- (getf *qf* :point)))
       (nodes (getf *qf* :nodes))
       (path (find-path-to-node pos nodes))
       (outer-node (caar path))
       (parent-node (car (before-last path)))
       (inner-node (car (alexandria:lastcar path))))
  (loop :for (node . index) :in path
	:for i :from 0
	:do (format t "~%=== Path part #~d, index ~d ===~%~s"
		    i index node))
  (format t "~%innore-node source: ~d-~d"
	  (node-start inner-node)
	  (node-end inner-node))
  (format t "~%unparsed inner-node: ~s"
	  (breeze.reader:unparse-to-string inner-node))
  (format t "~%nearest in-package: ~a" (find-nearest-in-package-form nodes outer-node))
  (format t "~%parent node: ~a" parent-node))

#+(or) (in-package-form-p
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
	 ;; Emacs's point starts at 1
	 (position (1- point))
	 (path (find-path-to-node position nodes))
	 ;; Find the top-level form "at point"
	 (outer-node (caar path))
	 ;; Find the innermost form "at point"
	 (inner-node (car (alexandria:lastcar path)))
	 (inner-node-index (cdr (alexandria:lastcar path)))
	 ;; Find the innermost form's parent
	 (parent-node (car (before-last path)))
	 ;; Accumulate a list of commands that make sense to run in
	 ;; the current context
	 (commands))
    (declare (ignorable inner-node inner-node-index parent-node))
    ;; When the buffer is empty, or only contains comments and
    ;; whitespaces.
    (when (emptyp nodes)
      (setf commands
	    (append *commands-applicable-at-toplevel* commands)))
    (when (or
	   ;; in-between forms
	   (null outer-node)
	   ;; just at the start or end of a form
	   (= position (node-start outer-node))
	   (= position (node-end outer-node))
	   ;; inside a comment (or a form disabled by a
	   ;; feature-expression)
	   (typep outer-node
		  'breeze.reader:skipped-node))
      (setf commands
	    (append *commands-applicable-at-toplevel* commands)))
    (when
	(or
	 (loop-form-p parent-node)
	 (loop-form-p inner-node))
      (setf commands
	    (append commands *commands-applicable-in-a-loop-form*)))
    ;; Print debug information
    (when t
      (when outer-node
	(format *debug-io* "~&Current top-level node's raw: ~s"
		(breeze.reader:node-raw outer-node)))
      #+ (or)
      (format *debug-io* "~&Current node: ~a"
	      (breeze.reader:unparse-to-string outer-node))
      ;; (format *debug-io* "~&Is current node a defpackage ~a" (defpackage-node-p outer-node))
      (format *debug-io* "~&position ~a" position)
      (format *debug-io* "~&positions ~a" (mapcar #'node-source nodes))
      ;; (format *debug-io* "~&nodes ~a" nodes)
      )
    ;; Deduplicate commands
    (setf commands (remove-duplicates commands :key #'car))
    ;; Save some information for debugging
    (setf *qf* `(,@all
		 :nodes ,nodes
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
