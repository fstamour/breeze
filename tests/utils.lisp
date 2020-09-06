(in-package #:common-lisp-user)

(defpackage #:breeze.utils.test
  (:use :cl)
  (:import-from #:breeze.test
                #:deftest
		#:is))

(in-package #:breeze.utils.test)

(deftest walk)

(deftest walk-list
  (is
    (equal
     '(('(mul))
       '(mul)
       (mul))
     (uiop:while-collecting (collect)
       (breeze.utils:walk-list
	'('(mul))
	#'(lambda (node)
	    (collect node))))))
  (is
    (equal
     '(('(a b) c (d e (f)))
       '(a b)
       (a b)
       (d e (f))
       (f))
     (uiop:while-collecting (collect)
       (breeze.utils:walk-list
	'('(a b) c (d e (f)))
	#'(lambda (node)
	    (collect node)))))))

(deftest walk-car
  (is (equal
       '('(a b) quote a d f)
       (uiop:while-collecting (collect)
	 (breeze.utils:walk-car
	  '('(a b) c (d e (f)))
	  #'(lambda (node)
	      (collect node)))))))

(deftest package-apropos)
(deftest optimal-string-alignment-distance)
(deftest indent-string)
(deftest print-comparison)
