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
