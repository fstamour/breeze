
(in-package #:breeze.listener)

(subtypep 'sb-int:simple-reader-package-error 'cl:package-error)

;; TODO a.k.a "Should signal"
(defmacro check-condition-type ((type-expected)
				&body body)
  `(let ((success nil))
     (ignore-errors
      (handler-bind
	  ((,type-expected
	     #'(lambda (condition)
		 (setf success t)))
	   ;; TODO FIXME ==> this is not called?
	   (error
	     #'(lambda (condition)
		 (error "Expected an error of type ~a, got a condition of type ~a"
			',type-expected
			(type-of condition)))))
	,@body))
     success))


(check-condition-type (#+sbcl sb-int:simple-reader-package-error)
  (read-from-string "(cl:prin)"))


(read-from-string "(cl:prin)")

(check-condition-type (#+sbcl sb-int:simple-reader-package-error)
  (read-from-string "(commmon-lisp:print :oups)"))

(check-condition-type (cl:package-error)
  (read-from-string "(cl:prin)"))

(check-condition-type (cl:undefined-function)
  (prin t))

(check-condition-type (#+sbcl sb-pcl:class-not-found-error)
		      (make-instance 'typos))


(defpackage #:foo (:use) (:export #:bar))
(handler-bind
    ((simple-condition ;; simple-warning
       #'(lambda (condition)
	   (describe condition))))
  (defpackage #:foo (:use)))

SB-INT:PACKAGE-AT-VARIANCE


use-package

(package-use-list (find-package :cl-user))
#|
(#<PACKAGE "COMMON-LISP"> #<PACKAGE "SB-ALIEN"> #<PACKAGE "SB-DEBUG">
#<PACKAGE "SB-EXT"> #<PACKAGE "SB-GRAY"> #<PACKAGE "SB-PROFILE">)
|#

defpackage


(handler-bind
    ((simple-condition ;; simple-warning
       #'(lambda (condition)
	   (describe condition))))
  (eval
   `(lambda ()
      (this-in-an-undefinded-function))))
;; #<SB-INT:SIMPLE-STYLE-WARNING ... >#


;; QUICKLISP-CLIENT:SYSTEM-NOT-FOUND
