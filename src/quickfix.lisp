
(in-package #:common-lisp-user)

(defpackage #:breeze.quickfix
  (:use :cl)
  (:export #:quickfix)
  (:import-from #:breeze.refactor
		#:form-at-point))

(in-package #:breeze.quickfix)

(defun quickfix (&rest all
		 &key
		   buffer-string
		   buffer-name
		   buffer-file-name
		   point
		   point-min
		   point-max)
  (form-at-point buffer-string point))
