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


;;; Trying to design a DSL for small refactors

(defpackage #:breeze.refactor-scratch
  (:documentation "")
  (:use #:cl))

(in-package #:breeze.refactor-scratch)

(defparameter *syntaxes* (make-hash-table :test 'equal)
  "Stores all the \"syntax definitions\".")

(defun variable-p (x)
  (and (symbolp x)
       (char= #\? (char (symbol-name x) 0))))

(defstruct (var
            (:constructor var (name))
            :constructor)
  (name nil :type symbol :read-only t))

(defstruct (constrained-var
            (:constructor the-var (constraints name))
            :constructor
            (:include var))
  (constraints nil :read-only t))

;; (var 'hey)
;; (the-var 'ho 'symbolp)

(defstruct (maybe :constructor) (pattern nil :read-only t))
(defstruct (zero-or-more :constructor) (pattern nil :read-only t))
(defstruct (alternation :constructor) (pattern nil :read-only t))

(defun list-vector (list)
  "Recursively convert a list into a vector."
  (if (atom list) list
      (map 'vector #'list-vector list)))

(defun compile-syntax (tree)
  (if (listp tree)
      (case (car tree)
        (the (apply #'the-var (rest tree)))
        (:zero-or-more (make-zero-or-more :pattern (compile-syntax (rest tree))))
        (:maybe (make-maybe :pattern (compile-syntax (rest tree))))
        (:alternation (make-alternation :pattern (compile-syntax (rest tree))))
        (otherwise (mapcar #'compile-syntax tree)))
      (if (variable-p tree) (var tree) tree)))

(compile-syntax '?x)
(compile-syntax '(the symbol ?x))
(compile-syntax 'x)
(compile-syntax '42)
(compile-syntax '(:maybe ?x))
(compile-syntax '((:maybe ?x)))

(trace compile-syntax variable-p)

(defmacro defsyntax (name &body body)
  `(setf (gethash ',name *syntaxes*)
         ',(list-vector
            (compile-syntax
             (if (breeze.utils:length>1? body)
                 body
                 (first body))))))



(defsyntax optional-parameters
  &optional
  (:zero-or-more
   (:alternation (the symbol ?var)
                 ((the symbol ?var)
                  ?init-form (:maybe (:symbol ?supplied-p-parameter))))))

(defsyntax rest-parameter &rest ?var)

(defsyntax body-parameter &body ?var)

(defsyntax key-parameters
  &key
  (:zero-or-more
   (:alternation (:symbol ?var)
                 ((:symbol ?var)
                  ?init-form (:maybe (:symbol ?supplied-p-parameter))))))

(defsyntax aux-parameters
  &aux
  (:zero-or-more
   (:alternation (:symbol ?var)
                 ((:symbol ?var) ?init-form)))
  (:maybe &allow-other-keys))

;; p.s. "match" is not implemented, it should look a lot like
;; unification; maybe I should look into using trivia?
#++
(match 'optional-parameters
  '(&optional))

(list
 '(&optional x)
 '(&optional (x 1))
 '(&optional (x 1 supplied-p))
 '(&optional x y (z t)))

(defsyntax ordinary-lambda-list
  (:zero-or-more ?var)
  optional-parameters
  rest-parameter
  key-parameters
  aux-parameters)

(defsyntax defun
  defun (:symbol ?name) ordinary-lambda-list (:body ?body))

#|

So far, I have the following cases:
- symbols that starts with ?
- list that starts with
-   :zero-or-more
-   :maybe
-   :alternation
-   :symbol
-   :body
- other lists

|#


;;; Imagine I have a structure that looks like this:

`(defun <ws> foo <ws> (x <ws> y) <nl> <ws> (+ <ws> x <ws> y))
;; where <ws> stands for whitespaces and <nl> for newlines

;; To match it against

`(defun ?name ?ordinary-lambda-list ?body)

;; I _cannot_ iterate over both in at the same speed.

;; Proof of concept:
(flet ((var-p (symbol)
         (char= #\? (char (symbol-name symbol) 0)))
       (skip-p (x)
         (and (symbolp x)
              (char= #\< (char (symbol-name x) 0)))))
  (let ((pattern
          (list-vector `(defun ?name ?ordinary-lambda-list ?body)))
        (input
          (list-vector `(defun <ws> foo <ws> (x <ws> y) <nl> <ws> (+ <ws> x <ws> y)))))
    (let ((i 0)
          (j 0))
      (flet ((advance-input ()
               (loop
                 :do (incf j)
                 :while (and (< j (length input))
                             (skip-p (aref input j))))))
        (loop
          :for guard :below 1000
          :while (< i (length pattern))
          :while (< j (length input))
          :collect (cons (aref pattern i)
                         (aref input j))
          :do (incf i)
              (advance-input)

          )))))


(defmethod skip-input-p (x)
  (and (symbolp x)
       (char= #\< (char (symbol-name x) 0))))

(defmethod match (pattern input)
  (if (variable-p pattern)
      ;; second value is the new binding
      (values t (cons pattern input))
      (equal pattern input)))

(match 'x 'x)
(match 'x 'y)
(match 2 2)

(match '?x 2)

(defmethod match ((pattern cons) input)
  (if (variable-p pattern)
      ;; second value is the new binding
      (values t (cons pattern input))
      (equal pattern input)))

(match '(:symbol x) 'y)

(defmethod match ((pattern vector) (input vector)
                  &aux (i 0) (j 0))
  (labels
      ((pattern () (aref pattern i))
       (input () (aref input j))
       (advance-pattern () (incf i))
       (advance-input ()
         (loop
           :do (incf j)
           :while (and (< j (length input))
                       (skip-input-p (input))))))
    (loop
      :for guard :below 1000
      :for (match . bindings) = (multiple-value-list
                                 (match (pattern) (input)))
      :unless match
        :return nil
      :append bindings
      :do (advance-pattern)
      :while (< i (length pattern))
      :do (advance-input)
      :while (< j (length input)))))


(let ((pattern
        (list-vector `(defun ?name ?ordinary-lambda-list ?body)))
      (input
        (list-vector `(defun <ws> foo <ws> (x <ws> y) <nl> <ws> (+ <ws> x <ws> y)))))
  (match pattern input))
