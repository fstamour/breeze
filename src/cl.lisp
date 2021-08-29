;;;; In this package, we're trying to add metadata to cl package's
;;;; symbols.
;;;;
;;;; See fndb.lisp and knownfn.lisp in sbcl

(in-package #:common-lisp-user)

(defpackage #:breeze.cl
  (:use :cl)
  (:import-from :breeze.xref
		#:generic-method-p
		#:specialp
		#:macrop
		#:simple-function-p
		#:classp)
  (:import-from :breeze.util
		#:breeze-relative-pathname))

(in-package #:breeze.cl)

;; (defvar *cl-symbols* (make-hash-table :test #'equalp :size 1000))

(defun lambda-list-keyword-p (symbol)
  (and (member symbol cl:lambda-list-keywords) t))

(defparameter *symbols*
  (flet ((pred (predicate symbol &optional name)
	   (when (funcall predicate symbol)
	     `(,(or name (alexandria:make-keyword predicate))))))
    (loop for symbol being the external-symbols of (find-package 'cl)
	  ;; do (setf (gethash (symbol-name symbol) *cl-symbols*))
	  collect
	  `(,symbol ;; Does the symbol have function?
	    ,@(pred 'fboundp symbol)
	    ;; Is the symbol a lamba list keyword? (e.g. &optional, &key, etc.)
	    ,@(pred 'lambda-list-keyword-p symbol)
	    ;; Is the symbol a variable?
	    ,@(pred 'boundp symbol)
	    ;; Is the symbol a type-specifier
	    #+sbcl
	    ,@(pred 'sb-ext:valid-type-specifier-p symbol :type-specifier)
	    ;; Does the symbol represent a class?
	    ,@(pred 'classp symbol :class)))))

;; (ql:quickload 'cl-form-types) ; not in quicklisp yet...

(setf (gethash "test" *cl-symbols*) :42)
(gethash "TEST" *cl-symbols*)


(let ((*print-case* :downcase))
  (print 'hi))

(alexandria:with-output-to-file (*standard-output* (breeze-rel)))
(loop :for (symbol . properties) :in *symbols*
      :do (print symbol))
