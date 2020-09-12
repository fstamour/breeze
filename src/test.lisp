
(uiop:define-package #:breeze.test
  (:documentation "Provides a test framework.")
  (:mix :cl #:alexandria #:breeze.definition)
  (:export
   #:*test*
   #:*test-change-hooks*
   #:deftest
   #:test
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
     (setf (gethash ',name *test*) (make-test ',name *package*
					      ,(when body
						 `'(progn ,@body))))
     (flag-test-change ',name)
     nil))

(defun test-body (name)
  "Get the body of a test by name"
  (if-let (body (gethash name *test*))
    (destructuring-bind (_ package body)
	body
	(declare (ignore _ package))
      (rest body))))

(defun run-test (name)
  "Run a test by name, returns a list containing a boolean and a condition."
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

(defun test (name &optional (message-on-success  "~&Passed."))
  "Run a test by name, report nicely."
  (destructuring-bind (passed condition)
      (run-test name)
    (if passed
        (format t message-on-success)
        (format t "~&Test \"~a\" failed with condition:~%\"~a\""
                name condition))
    passed))


;; This is ok, but it doesn't have a clear report
(defun run-all-tests (&optional test-list)
  "Run all the tests"
  (format t "~&Running tests...")
  (let ((failed 0)
                (total 0))
    (loop :for name :in (or test-list (hash-table-keys *test*))
          :for passed = (test name ".")
          :do
             (unless passed
               (incf failed))
             (incf total)
          :finally (format t "~&Done [~d/~d] tests passed.~%" (- total failed) total)
                   (force-output))
    (values (if (zerop failed)
                nil
                failed)
            total)))

(defmacro is (&body body)
  "Macro that signals an error when its body evaluate to nil"
  `(unless (progn ,@body)
     (error "Expression is falsy: ~A" '(progn ,@body))))
