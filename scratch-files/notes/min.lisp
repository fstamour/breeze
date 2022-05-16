;;; This is just an example used to test the parsing of source code

(in-package #:common-lisp-user)

(defpackage #:min
  (:use :cl))

(in-package #:min)

;; A less powerful min!
(defun min2 (a b)
  (cl:min a b))
