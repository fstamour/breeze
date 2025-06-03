(in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.utils
    (:documentation "Tests for breeze.test.")
  (:mix #:cl #:alexandria #:breeze.utils)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false))

(in-package #:breeze.test.utils)

(define-test package-apropos)


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

(define-test length>1?
  (false (length>1? nil))
  (false (length>1? '(a)))
  (true (length>1? '(a b))))
