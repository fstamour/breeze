(in-package #:common-lisp-user)

(uiop:define-package #:breeze.utils.test
    (:documentation "Tests for breeze.test.")
    (:mix #:cl #:alexandria #:breeze.utils)
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
       (walk-list
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
       (walk-list
	'('(a b) c (d e (f)))
	#'(lambda (node)
	    (collect node)))))))

(deftest walk-car
  (is (equal
       '('(a b) quote a d f)
       (uiop:while-collecting (collect)
	 (walk-car
	  '('(a b) c (d e (f)))
	  #'(lambda (node)
	      (collect node)))))))

(deftest package-apropos)
(deftest optimal-string-alignment-distance)
(deftest indent-string)
(deftest print-comparison)


(deftest read-stream-range
  (is (equal
       (multiple-value-list
	(with-input-from-string
	 (stream "(1 #|comment|# \"string\")")
	 (values
	  (read-stream-range stream 3 (+ 3 11))
	  (file-position stream))))
       '("#|comment|#" 0))))

(deftest stream-size
  (is (= 24
	 (with-input-from-string
	  (stream "(1 #|comment|# \"string\")")
	  (stream-size stream)))))
