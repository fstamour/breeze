(in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.utils
    (:documentation "Tests for breeze.test.")
  (:mix #:cl #:alexandria #:breeze.utils)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false))

(in-package #:breeze.test.utils)

(define-test walk)

(define-test walk-list
  (is equal
      '(('(mul)) '(mul) (mul))
      (uiop:while-collecting (collect)
        (walk-list
         '('(mul))
         #'(lambda (node)
             (collect node)))))
  (is equal
      '(('(a b) c (d e (f)))
        '(a b)
        (a b)
        (d e (f))
        (f))
      (uiop:while-collecting (collect)
        (walk-list
         '('(a b) c (d e (f)))
         #'(lambda (node)
             (collect node))))))

(define-test walk-car
  (is equal
      '('(a b) quote a d f)
      (uiop:while-collecting (collect)
        (walk-car
         '('(a b) c (d e (f)))
         #'(lambda (node)
             (collect node))))))

(define-test package-apropos)
(define-test optimal-string-alignment-distance)
(define-test indent-string)
(define-test print-comparison)


(define-test read-stream-range
  (is equal
      (multiple-value-list
       (with-input-from-string
           (stream "(1 #|comment|# \"string\")")
         (values
          (read-stream-range stream 3 (+ 3 11))
          (file-position stream))))
      '("#|comment|#" 0)))

(define-test stream-size
  (is = 24
      (with-input-from-string
          (stream "(1 #|comment|# \"string\")")
        (stream-size stream))))

(define-test before-last
  (false (before-last '()))
  (false (before-last '(a)))
  (is eq 'a (before-last '(a b)))
  (is eq 'b (before-last '(a b c))))

#+nil
(minimizing (x)
  (x 'a 10)
  (x 'b 5))
;; => B, 5
#+nil
(minimizing (x)
  (x 'a nil))
#+nil
(minimizing (x :tracep t)
  (x 'a 10))

#+ (or)
(optimal-string-alignment-distance*
 "breeze.util"
 "breeze.utils"
 3)

(define-test length>1?
  (false (length>1? nil))
  (false (length>1? '(a)))
  (true (length>1? '(a b))))
