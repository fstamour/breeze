
(defpackage #:breeze.test
  (:use :cl #:alexandria)
  (:export
   #:*test*
   #:*test-change-hooks*
   #:deftest
   #:run-test
   #:run-all-tests
   #:is
   #:test-body))

(in-package #:breeze.test)

(defvar *test* (make-hash-table)
  "Set of all tests defined with breeze.test:deftest")

(defvar *test-change-hooks* ()
  "List of functions to call when a test is redefined")

(defun flag-test-change (name)
  "Called when a test is re-defined"
  (loop :for hook :in *test-change-hooks*
        :do (funcall hook name)))

(defun flag-failed-test (name body condition)
  (declare (ignore body))
  (warn "~&Test ~A failed with condition ~A~%" name condition))

(defun make-test (name package body)
  "Create an object of type test"
  (list name package body))

(defmacro deftest (name &body body)
  "Defines a test"
  (check-type name symbol)
  `(progn
     (setf (gethash ',name *test*) (make-test ',name *package* '(progn ,@body)))
     (flag-test-change ',name)
     nil))

(defun test-body (name)
  "Get the body of a test by name"
  (destructuring-bind (_ package body)
      (gethash name *test*)
    (declare (ignore _ package))
    (rest body)))

(defun run-test (name)
  "Run a test by name, returns a list (passed condition)"
  (destructuring-bind (_ package body)
      (gethash name *test*)
    (declare (ignore _))
    (let ((passed nil)
          (condition nil))
      (with-output-to-string (*standard-output*)
        (handler-case (progn
                        ;; (format *debug-io* "~&Running test ~A~%" name)
                        (let ((*package* package))
                          (eval body))
                        (setf passed t))
          (error (c) (setf condition c))))
      (unless passed
        (flag-failed-test name body condition))
      (list passed condition))))

;; This is ok, but it doesn't have a clear report
(defun run-all-tests (&optional test-list)
  "Run all the tests"
  (format t "~&Running all tests...")
  (loop :for name :in (or test-list (hash-table-keys *test*))
        ;; :collect (list name (run-test name))
        :do (destructuring-bind (passed condition)
                (run-test name)
                (unless passed
                  (format t "~&Test \"~a\" failed with condition:~%\"~a\""
                          name condition))))
  (format t "~&Done."))

(defmacro is (&body body)
  "Macro that signals an error when its body evaluate to nil"
  `(unless (progn ,@body)
     (error "Expression is falsy: ~A" '(progn ,@body))))


