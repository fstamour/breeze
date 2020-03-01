
(uiop:define-package #:breeze.selftest
    (:use #:breeze.user #:alexandria)
  (:import-from #:breeze.test
                #:*test*)
  (:shadowing-import-from #:cl #:defun)
  (:export #:selftest))

(in-package #:breeze.selftest)


;;; Defines a few dummy functions

(breeze.definition:defun mul (x y)
  (* x y))

(breeze.definition:defun 2x (x)
  (mul 2 x))

(breeze.definition:defun add-one (x)
  (1+ x))

(deftest mul
  (is (= 4 (mul 2 2)))
  (is (= 12 (mul 2 6))))

(deftest 2x
  (is (= (2x 2) (mul 2 2))))

#+nil
(deftest should-fail
  (is (= 6 (mul 2 2))))



(defun selftest-p (test-name)
  "Is this test a self-test?"
  (starts-with-subseq (symbol-name 'self/)
                      (symbol-name test-name)))

(defun selftest ()
  (run-all-tests
   (loop :for test-name :being :the :hash-key :of *test*
         :when (selftest-p test-name)
           :collect test-name)))



(defun test-calls-who* (test-name)
  (remove-if #'selftest-p (test-calls-who test-name)))

(deftest self/test-calls-who
  (is (equalp (test-calls-who* 'mul) '(mul mul))))

(defun tested-by* (function-name)
  (remove-if #'selftest-p (tested-by function-name)))

(deftest self/tested-by
  (is (equalp (tested-by* 'mul) '(mul 2x #+nil should-fail))))

(deftest self/test-case
  (is (equal '((mul 2 6) (mul 2 2)) (test-case 'mul))))

;; (breeze.xref::function-without-test)



(deftest self/calls-who
    (is (equalp (calls-who '2x) '(mul))))



;; (selftest)


#+nil
(breeze.utils:walk-list
 '('(mul))
 #'(lambda (node)
     (let ((p (eq 'quote (car node))))
       (format t "~&~A ~A%" p node))))

