
(defpackage #:breeze.xref
  (:documentation "This package contains function for cross-referencing.")
  (:use :cl #:breeze.utils #:alexandria)
  (:import-from #:breeze.definition
                #:*function*
                #:function-body)
  (:import-from #:breeze.test
                #:test-body
                #:*test*)
  (:export
   #:calls-who
   #:test-calls-who
   #:tested-by
   #:test-case))

(in-package #:breeze.xref)

(defun calls-who (function-name)
  "Take a function name and returns a list of all the functions it calls."
  (with-collector
      (walk-car
       (function-body function-name)
       (lambda (el)
         (when (function-body el)
           (collect el))))))

(defun test-calls-who (test-name)
  "Take a test name and return a list of all the functions it calls."
  (with-collector
    (walk-car
     (test-body test-name)
     #'(lambda (el)
         (when (function-body el)
           (collect el))))))

(defun tested-by (function-name)
  "Take a function name and return a list of all the tests that calls it."
  (loop :for test-name :being :the :hash-key :of *test*
        :when (member function-name (test-calls-who test-name))
          :collect test-name))

(defun test-case (function-name)
  "List all the test-cases"
  (let ((case-set (make-hash-table :test 'equal)))
    (loop :for test-name :being :the :hash-key :of *test*
          :for test-body = (test-body test-name)
          :do (walk-list
               test-body
               #'(lambda (list)
                   (when (eq function-name (car list))
                     (setf (gethash list case-set) t)))
               #'(lambda (node)
                   (if (listp node)
                       (not (eq 'quote (car node)))
                       t))))
    (hash-table-keys case-set)))

(defun function-without-test (&optional (package *package*))
  ""
  (let ((function-called (make-hash-table)))
    (loop :for test-name :being :the :hash-key :of *test*
          :do (loop :for function-name :in (test-calls-who test-name)
                    :do (setf (gethash function-name function-called) t)))
    (loop :for function-name :being :the :hash-key :of *function*
          :unless (gethash function-name function-called)
            :collect function-name)))

#+wip ;; It's done, but not tested or used
(defun list-function (&optional (package *package*))
  "List all the functions, optionally filter by package"
  (loop :for function-name :being :the :hash-key :of *function*
        :when (if package
                  (eq package (symbol-package function-name))
                  t)
          :collect function-name))

#+wip ;; Look at parse-smth in cover.lisp
(defun function-without-documentation (&optional (package *package*)))

