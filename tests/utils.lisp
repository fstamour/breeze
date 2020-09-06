(in-package #:common-lisp-user)

(defpackage #:breeze.utils.test
  (:use :cl)
  (:import-from #:breeze.test
                #:deftest
		#:is))

(in-package #:breeze.utils.test)

#+nil
(breeze.utils:walk-list
 '('(mul))
 #'(lambda (node)
     (let ((p (eq 'quote (car node))))
       (format t "~&~A ~A%" p node))))

(deftest walk)
(deftest walk-list)
(deftest walk-car)
(deftest package-apropos)
(deftest optimal-string-alignment-distance)
(deftest indent-string)
(deftest print-comparison)
