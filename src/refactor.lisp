(in-package #:common-lisp-user)

(uiop:define-package #:breeze.refactor
    (:use #:cl #:breeze.command)
  (:import-from
   #:alexandria
   #:if-let)
  (:import-from
   #:breeze.utils
   #:symbol-package-qualified-name
   #:before-last
   #:positivep
   #:find-version-control-root)
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
  "Helper macro to define lots of predicates."
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
  "Given a list of NODES, return which node contains the POSITION."
  (loop :for node :in nodes
	:for (start . end) = (node-source node)
	:for i :from 0
	:when (and
	       (<= start position end)
	       (<= position end))
	  :do
	     (return (cons node i))))

(defun find-path-to-node (position nodes)
  "Given a list of NODES, return a path (list of cons (node . index))"
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
  "Helper macro to define lots of predicates."
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
  "Helper macro to define lots of predicates."
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
  "Helper macro to define lots of predicates."
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
     lambda
     map
     mapcar
     mapcond))

;; TODO A "skipped node" can also be a form hidden behing a feature (#+/-)
(defun emptyp (nodes)
  "Whether a list of node contains no code."
  (every #'(lambda (node)
	     (typep node 'breeze.reader:skipped-node))
	 nodes))


;; TODO symbol-qualified-p : is the symbol "package-qualified"



;;; Insertion commands

;; Dogfood'ing to the max!
(define-command insert-breeze-define-command ()
  "Insert a breeze:define-command form."
  (read-string "Name of the command (symbol): ")
  (handle (name)
    (insert
     "(define-command ~a ()~
    ~%  \"~@(~a~).\"~
    ~%  )"
     name
     (substitute #\Space #\- name))))

(define-command insert-handler-bind-form ()
  "Insert handler bind form."
  (insert
   "(handler-bind
      ((error #'(lambda (condition)
		  (describe condition *debug-io*))))
    (frobnicate))"))

(define-command insert-loop-clause-for-on-list ()
  "Insert a loop clause to iterate on a list."
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :on ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(define-command insert-loop-clause-for-in-list ()
  "Insert a loop clause to iterate in a list."
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :in ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(define-command insert-loop-clause-for-hash ()
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

(defun insert-defvar-shaped (form-name)
  "Start a command to insert a form that has the same shape as a
defvar."
  (insert "(~a " form-name)
  ;; TODO Check if name is surrounded by "*"
  (read-string-then-insert "Name: " "*~a* ")
  (read-string-then-insert "Initial value: " "~a~%")
  (read-string-then-insert "Documentation string " "\"~a\")"))

(define-command insert-defvar ()
  "Insert a defvar form."
  (insert-defvar-shaped "defvar"))

(define-command insert-defparameter ()
  "Insert a defparameter form."
  (insert-defvar-shaped "defparameter"))

(define-command insert-defconstant ()
  "Insert a defconstant form."
  (insert-defvar-shaped "defconstant"))

;; TODO Add "alexandria" when the symbol is not interned
;;      ^^^ that should go in "refactor.lisp"
(define-command insert-define-constant ()
  "Insert a alexandria:define-constant form."
  (insert-defvar-shaped "define-constant"))


(defun insert-defun-shaped (form-name)
  "Start a command to insert a form that has the same shape as a
defun."
  (insert "(~a " form-name)
  (read-string-then-insert "Name: " "~a (")
  (read-string-then-insert
   ;; Who needs to loop...?
   "Enter the arguments: " "~a)~%)")
  (backward-char))

(define-command insert-defun ()
  "Insert a defun form."
  (insert-defun-shaped "defun"))

(define-command insert-defmacro ()
  "Insert a defmacro form."
  (insert-defun-shaped "defmacro"))

(defparameter *insert-defpackage/cl-user-prefix* nil
  "Whether to include (in-package #:cl-user) before a defpackage form.")

(defun directory-name (pathname)
  (alexandria:lastcar (pathname-directory pathname)))

(defun infer-project-name (path)
  "Try to infer the name of the project from the PATH."
  ;; Infer project-name by location .git folder
  (when path
    (if-let ((vc-root (find-version-control-root path)))
      (directory-name vc-root))))

(defun infer-is-test-file (path)
  "Try to infer if a file is part of the tests."
  (when path
    (member
     (directory-name path)
     '("test" "tests" "t")
     :test #'string-equal)))

(defun infer-package-name-from-file (file-pathname)
  "Given a FILE-PATHNAME, infer a proper package name."
  (when file-pathname
    (let ((project (infer-project-name file-pathname))
	  (test (when (infer-is-test-file file-pathname)
		  "test"))
	  (name (pathname-name file-pathname)))
      (format nil "~{~a~^.~}"
	      (remove-if #'null (list project test name))))))

#+ (or)
(trace
 infer-project-name
 infer-is-test-file
 infer-package-name-from-file)

(define-command insert-defpackage ()
  "Insert a defpackage form."
  (read-string "Name of the package: "
	       (infer-package-name-from-file buffer-file-name))
  (handle (package-name)
    (when *insert-defpackage/cl-user-prefix*
      (insert
       "(cl:in-package #:cl-user)~%~%~"))
    (insert
     "(defpackage #:~a~
    ~%  (:use #:cl)~
    ~%  (:documentation \"\"))~
    ~%~
    ~%(in-package #:~a)"
     package-name package-name)))

(define-command insert-local-nicknames ()
  "Insert local nicknames."
  (insert
   "(:local-nicknames ~{~a~^~%~})"
   (loop :for name =
	 ;; TODO This form is not ideal...
		   (progn (read-string "Name of the package to alias: ")
			  (handle (name) name))
	 :while (positivep (length name)) ; TODO there's a better function for that somewhere too..
	 :for alias = (progn (read-string "Alias of the package: ")
			     (handle (name) name))
	 :collect (format nil "(#:~a #:~a)" alias name))))

(define-command insert-in-package-cl-user ()
  "Insert (cl:in-package #:cl-user)"
  (insert "(cl:in-package #:cl-user)"))

;; TODO insert-let (need to loop probably)

(define-command insert-asdf ()
  "Insert an asdf system definition form."
  (read-string "Name of the system: ")
  (handle (system-name)
    (read-string "Author: ")
    (handle (author)
      (read-string "Licence name: ")
      (handle (licence)
	(insert "(cl:in-package #:cl)~%~%")
	;; TODO don't insert a defpackage if it already exists
	(insert "(defpackage #:~a.asd~%  (:use :cl :asdf))~%~%"
		system-name)
	(insert "(in-package #:~a.asd)~%~%" system-name)
	(insert "(asdf:defsystem #:~a~%~{  ~a~%~}"
		system-name
		`(":description \"\""
		  ":version \"0.0.1\""
		  ,(format nil ":author \"~a\"" author)
		  ,(format nil ":licence \"~a\"" licence)
		  ":depends-on '()"
		  ";; :pathname src"
		  ":serial t"
		  "  :components
    (#+(or) (:file \"todo.lisp\")))"))))))



;; TODO How could I re-use an hypothetical "insert-slot" command?
(define-command insert-defclass ()
  "Insert a defclass form."
  (read-string-then-insert
   "Name of the class: "
   "(defclass ~a ()~
   ~%  ((slot~
   ~%    :initform nil~
   ~%    :initarg :slot~
   ~%    :accessor ~@*~a-slot))~
   ~%  (:documentation \"\"))"))

(define-command insert-defgeneric ()
  "Insert a defgeneric form."
  (read-string-then-insert
   "Name of the generic: "
   "(defgeneric ~a ()~
   ~%  (:documentation \"\")~
   ~%  (:method () ()))"))

(define-command insert-print-unreadable-object-boilerplate ()
  "Insert a print-object method form."
  (read-string
   "Name of the object (paramater name of the method): ")
  (handle (name)
    (read-string
     "Type of the object: ")
    (handle (type)
      (insert
       "(defmethod print-object ((~a ~a) stream)~
          ~%  (print-unreadable-object~
          ~%      (~a stream :type t :identity nil)~
          ~%    (format stream \"~~s\" (~a-something ~a))))"
       name type
       name
       type name))))


(define-command insert-lambda ()
  "Insert a lambda form."
  (insert "#'(lambda ())"))

;; TODO quick-insert (format *debug-io* "~&")


;;; Quickfix

(defgeneric describe-command (command)
  (:documentation "Give a user-friendly description for a command.")
  (:method ((command symbol))
    (symbol-package-qualified-name command))
  (:method ((command (eql 'insert-defpackage)))
    "Insert a defpackage form."))

(defun command-description (function)
  "Return a list of 2 elements: (FUNCTION its-docstring)"
  (let ((doc (documentation function 'function)))
    (unless doc
      (error
       "Function ~s does not have a documentation string.~
                  Is it defined?"
       function))
    (list function doc)))

(defparameter *commands-applicable-at-toplevel*
  '(insert-asdf
    insert-defun
    insert-defmacro
    insert-defpackage
    insert-in-package-cl-user
    insert-defvar
    insert-defparameter
    insert-defclass
    insert-defgeneric
    insert-print-unreadable-object-boilerplate
    insert-breeze-define-command))

(defparameter *commands-applicable-in-a-loop-form*
  '(insert-loop-clause-for-in-list
    insert-loop-clause-for-on-list
    insert-loop-clause-for-hash))

;; That's some Java-level variable name
(defparameter *commands-applicable-inside-another-form-or-at-toplevel*
  '(insert-handler-bind-form))


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

(defun augment-context-by-parsing-the-buffer (context)
  (let* ((buffer-string (context-buffer-string context))
	 ;; Emacs's point starts at 1
	 (position (1- (context-point context)))
	 ;; Parse the buffer
	 (nodes (parse-string buffer-string))
	 (path (find-path-to-node position nodes))
	 ;; Find the top-level form "at point"
	 (outer-node (caar path))
	 ;; Find the innermost form "at point"
	 (inner-node (car (alexandria:lastcar path)))
	 (inner-node-index (cdr (alexandria:lastcar path)))
	 ;; Find the innermost form's parent
	 (parent-node (car (before-last path))))
    #. `(progn ,@(loop :for key in '(position nodes path outer-node
				     inner-node inner-node-index parent-node)
		       :collect
		       `(context-set context ',key ,key)))))

(defun in-package-node-package (in-package-node)
  (node-content
   (second (node-content in-package-node))))

(defun validate-nearest-in-package (nodes outer-node)
  (let* ((previous-in-package-form
	   (find-nearest-sibling-in-package-form nodes outer-node)))
    (when previous-in-package-form
      (let* ((package-designator (in-package-node-package
				  previous-in-package-form))
	     (package (find-package package-designator)))
	(when (null package)
	  package-designator)))))


(define-command quickfix ()
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
	 (invalid-in-package (validate-nearest-in-package nodes outer-node))
	 ;; Accumulate a list of commands that make sense to run in
	 ;; the current context
	 (commands))
    (declare (ignorable inner-node inner-node-index parent-node))

    (labels ((append-commands (cmds)
	       (setf commands (append cmds commands)))
	     (push-command (fn)
	       (push fn commands))
	     (push-command* (&rest fns)
	       (mapcar #'push-command fns)))
      (cond
	;; When the previous in-package form desginate a package tha
	;; cannot be found (e.g. the user forgot to define a package.
	(invalid-in-package
	 (warn "The nearest in-package form designates a package that doesn't exists: ~s"	       invalid-in-package)
	 (return))
	;; When the buffer is empty, or only contains comments and
	;; whitespaces.
	((emptyp nodes)
	 (push-command*

	  'insert-defpackage))
	;; TODO Check if files ends with ".asd"
	;; (push-command 'insert-asdf)
	((or
	  ;; in-between forms
	  (null outer-node)
	  ;; just at the start or end of a form
	  (= position (node-start outer-node))
	  (= position (node-end outer-node))
	  ;; inside a comment (or a form disabled by a
	  ;; feature-expression)
	  (typep outer-node
		 'breeze.reader:skipped-node))
	 (append-commands *commands-applicable-at-toplevel*)
	 (append-commands
	  *commands-applicable-inside-another-form-or-at-toplevel*))
	;; Loop form
	((or
	  (loop-form-p parent-node)
	  (loop-form-p inner-node))
	 (append-commands *commands-applicable-in-a-loop-form*))
	;; Defpackage form
	((defpackage-form-p inner-node)
	 (push-command 'insert-local-nicknames))
	((and (mapcar-form-p inner-node)
	      )

	 (push-command 'insert-lambda))
	(t
	 (append-commands
	  *commands-applicable-inside-another-form-or-at-toplevel*))))

    ;; Deduplicate commands
    (setf commands (remove-duplicates commands))

    ;; Augment the commands with their descriptions.
    (setf commands (mapcar #'command-description commands))

    ;; Save some information for debugging
    (setf *qf* `((:context . ,context)
		 (:nodes . ,nodes)
		 (:path . ,path)
		 (:outer-node . ,outer-node)
		 (:inner-node . ,inner-node)
		 (:parent-node . ,parent-node)
		 (:commands . ,commands)))

    ;; Ask the user to choose a command
    (choose "Choose a command: "
	    (mapcar #'second commands))
    (handle (choice)
      (let ((command-function (car (find choice commands
					 :key #'second
					 :test #'string=))))
	(funcall command-function)))))


#+nil
(quickfix :buffer-string "   " :point 3)
