;;; Trying to refactor stuff

(in-package #:common-lisp-user)

(defpackage #:refactor-scratch
  (:use :cl)
  (:import-from #:breeze.reader
		#:node-content
		#:parse-string
		#:unparse-to-string

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
  (:import-from #:breeze.test
		#:deftest
		#:is))

(in-package #:refactor-scratch)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun node-first (node)
  (first (node-content node)))

(defun node-lastcar (node)
  (alexandria:lastcar (node-content node)))

(defun node-string-equal (string node)
  (string-equal string (node-content node)))

(defun null-node-p (node)
  (and (symbol-node-p node)
       (node-string-equal "nil" node)))

#+nil
(let ((node (first
	     (breeze.reader:parse-string "(if x nil)"))))
  (null-node-p
   (node-lastcar node)))

(defun node-length (node &optional (ignore-skipped-p t))
  (and (list-node-p node)
       (length
	(if ignore-skipped-p
	    (remove-if #'skipped-node-p (node-content node))
	    (node-content node)))))

(defun node-symbol= (symbol node)
  (and (symbol-node-p node)
       (node-string-equal (symbol-name symbol)
			  node)))

(defun if-p (node)
  (and (list-node-p node)
       (node-symbol= 'if (node-first node))))

(read-from-string "(if x nil)")

(defun test-refactor-if (string)
  (let ((node (first (parse-string string))))
    (when (and (if-p node)
	       (= 3 (node-length node)))
      (setf (node-content (first (node-content node)))
	    (if (null-node-p (node-lastcar node))
		"unless"
		"when"))
      (unparse-to-string (list node)))))

(test-refactor-if "(if x nil)")
"(unless x nil)"

(test-refactor-if "(if x 32)")
"(when x 32)"

(test-refactor-if
 "(if #| comment |# 'x NIL)")
"(unless #| comment |# 'x nil)"

(test-refactor-if
 "(IF
    'x NIL)")
"(unless
    'x nil)" ;; FIXME it should be "NIL"

(defun defpackage-node-p (node)
  (and
   (list-node-p node)
   (node-symbol= 'defpackage (node-first node))))

(let ((node (first
	     (breeze.reader:parse-string "(defpackage )"))))
  (defpackage-node-p node))


(defclass defpackage-node (list-node)
  ())

(typep (make-instance 'defpackage-node) 'list-node)
;; => t

;; ===> I could have a ton of specialized classes to help with manipulating the syntax tree

(let ((node (read-from-string "(defpackage name)")))
  (and
   (typep node 'list-node)
   (let ((car (car (node-content node))))
     (and (typep car 'symbol-node)
	  (string-equal "defpackage" (node-content car))))))


(parse-string "(defpackage name)")



;; Forms to add to defsystem to test with parachute
(let ((system-name "breeze"))
  (let ((test-system (format nil "~a/test" system-name))
	(test-package (format nil "~a/test" system-name))
	(test-function system-name))
    (format nil
	    "~{~A~}"
	    (list
	     ":in-order-to ((test-op (load-op #:" test-system")))
 :perform
   (test-op (o c)
   (symbol-call
    '#:parachute '#:test
    (find-symbol (symbol-name '#:" test-function ")
                 (find-package '#:" test-package "))
    :report (find-symbol \"INTERACTIVE\" \"PARACHUTE\")))"))))



(defun symbol-name-string= (string symbol)
  (and
   (stringp string)
   (symbolp symbol)
   (string= (symbol-name symbol) string)))

(defun with-clause-p (form)
  (when (listp form)
    (symbol-name-string= #.(symbol-name 'with) (car form))))

(with-clause-p '()) nil
(with-clause-p '(with)) t
(with-clause-p '(:with)) t

(defun check-with-clause (clause)
  (destructuring-bind (with binding =-symbol callback) clause
    (declare (ignore with)
	     (ignorable callback))
    (unless (symbolp binding)
      (error "A \"with\" clause expects a symbol ~
                            after the \"with\". Got ~s" binding))
    (unless (symbol-name-string= #.(symbol-name '=) =-symbol)
      (error "A \"with\" clause expects a = after the ~
                           name of the binding. Got ~s." =-symbol))))

(let ((context '()) ; context is actually a plist
      (body
	`((with system-name = (read-string
			       "Name of the system: "))
	  (with author = (read-string
			  "Author: "))
	  (with licence = (read-string
			   "Licence name: "))
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
    (#+(or) (:file \"todo.lisp\")))")))
	  ))
      (bindings nil)
      (bindings-tail nil))
  (loop
    :for clause :in body
    :for with-clause-p = (with-clause-p clause)
    :for new-binding = (when with-clause-p
			 (second clause))
    :for new-callback = (if with-clause-p
			    (fourth clause)
			    clause)
    :when with-clause-p
      :do (check-with-clause clause)
    :collect
    (let ((callback
	    (if with-clause-p
		(alexandria:with-gensyms (continuation x)
		  `(lambda (,continuation)
		     (,@new-callback
		      (lambda (,x)
			(setf (getf ',new-binding context) ,x)
			(funcall ,continuation)))))
		new-callback)))
      (prog1
	  `(symbol-macrolet
	       (,@(loop :for binding :in bindings
			:collect
			`(,binding (getf context ',binding))))
	     ,callback)
	;; Push the new binding
	(when new-binding
	  (if bindings
	      (setf (cdr bindings-tail) (cons new-binding nil)
		    bindings-tail (cdr bindings-tail))
	      (setf bindings (cons new-binding nil)
		    bindings-tail bindings)))))))


(defun %with-clause-binding (clause)
  "Extract the \"binding\" part from a \"with\" CLAUSE."
  (second clause))

(defun with-clause-binding (clause)
  "Extract the \"binding\" part from a \"with\" CLAUSE."
  (when (with-clause-p clause)
    (%with-clause-binding clause)))

(defun %with-clause-callback (clause)
  "Extract the \"callback\" part from a \"with\" CLAUSE."
  (fourth clause))

(defun with-clause-callback (clause)
  "Extract the \"callback\" part from a \"with\" CLAUSE."
  (when (with-clause-p clause)
    (%with-clause-callback clause)))

(defun transform-with-clause (clause context-var)
  (alexandria:with-gensyms (continuation x)
    `(lambda (,continuation)
       (,@ (%with-clause-callback clause)
	   (lambda (,x)
	     (setf (getf ',(%with-clause-binding clause) ,context-var)
		   ,x)
	     (funcall ,continuation))))))

(defun clause-callback (clause context-var)
  (cond
    ((with-clause-p clause)
     (check-with-clause clause)
     (transform-with-clause clause context-var))
    (t clause)))

(defmacro with-chaining ((context-var) &body body)
  `(list
    ,@(loop
	:for clause :in body
	:for new-binding = (with-clause-binding clause)
	:for callback = (clause-callback clause context-var)
	:collect
	`(symbol-macrolet
	     (,@(loop :for binding :in bindings
		      :collect
		      `(,binding (getf ,context-var ',binding))))
	   ,callback)
	:when new-binding
	  :collect new-binding :into bindings)))


(let ((context (list)))
  (with-chaining (context)
    (with system-name = (read-string
			 "Name of the system: "))
    (with author = (read-string
		    "Author: "))
    (with licence = (read-string
		     "Licence name: "))
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
    (#+(or) (:file \"todo.lisp\")))")))
    ))


(require 'asdf)
(asdf:map-systems #'print)
