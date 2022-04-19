
(uiop:define-package #:breeze.test.test
    (:documentation "Tests for breeze.test.")
  (:mix #:breeze.user #:cl #:alexandria)
  (:import-from #:breeze.test
                #:deftest
                #:is))

(in-package #:breeze.test.test)

(deftest test-body
  (is (not (test-body (gensym))))
  (is (equal
       '((is
           (= 4 (breeze.dummy.test:mul 2 2)))
         (is
           (= 12 (breeze.dummy.test:mul 2 6))))
       (test-body 'dum:mul))))
