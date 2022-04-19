(in-package #:common-lisp-user)

(uiop:define-package #:breeze.xref.test
    (:documentation "Tests for breeze.xref.")
  (:mix #:breeze.xref #:cl #:alexandria)
  (:import-from #:breeze.test
                #:deftest
                #:is))

(in-package #:breeze.xref.test)

(deftest test-calls-who
  (is (equalp '(dum:mul = is) (test-calls-who 'dum:mul))))

(deftest tested-by
  (is (member 'dum:mul (tested-by 'dum:mul)))
  (is (member 'dum:2x (tested-by 'dum:mul))))

(deftest test-case
  (is (equal '((dum:mul 2 6) (dum:mul 2 2)) (test-case 'dum:mul))))


(deftest package-test
  (is (equal '(dum:mul dum:2x) (package-test 'dum)))
  (is (equal (package-test 'dum) (package-test (find-package 'dum)))))

(deftest calls-who
  (is (equalp (calls-who 'dum:2x) '(dum:mul))))

;; (breeze.xref::function-without-test)
