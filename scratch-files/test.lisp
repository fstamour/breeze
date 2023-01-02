
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
   #:test-body
   #:test-results))

(in-package #:breeze.test)

(defvar *test* (make-hash-table)
  "Set of all tests defined with breeze.test:deftest")

(defvar *test-change-hooks* ()
  "List of functions to call when a test is redefined")

(defvar *test-results* (make-hash-table)
  "Results of the latest test run.")

(defvar *test-results-history* ()
  "History of the test runs.")

(defparameter *test-output-stream* (make-synonym-stream '*standard-output*))

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

(defclass test-result ()
  ((name
    :accessor name
    :initarg :name)
   (outcome
    :accessor outcome
    :initarg :outcome)
   (condition
    :accessor result-condition
    :initarg :condition)
   (time
    :accessor elapsed-time
    :initarg :time))
  (:documentation "Represent the result of running a test"))

(defmethod passedp (test-result)
  (eq (outcome test-result) :success))

;; RENDU: faire une méthode pour afficher les résultats (voir la function "test"
;; (defmethod describe )

(defun run-test (name)
  "Run a test by name, returns a test-result."
  (destructuring-bind (_ package body)
      (gethash name *test*)
    (declare (ignore _))
    (let ((passed nil)
          (condition nil)
          (start-time (get-internal-real-time))
          (end-time nil))
      (with-output-to-string (*standard-output*)
        (handler-case (progn
                        ;; (format *debug-io* "~&Running test ~A~%" name)
                        (let ((*package* package))
                          (eval body))
                        (setf passed t))
          (error (c) (setf condition c))))
      (setf end-time (get-internal-real-time))
      (unless passed
        (flag-failed-test name body condition))
      (make-instance 'test-result
                     :name name
                     :outcome (if passed :success :failure)
                     :condition condition
                     :time (- end-time start-time)))))

(defun test (name &optional (message-on-success  "~&Passed."))
  "Run a test by name, report nicely."
  (let ((result (run-test name)))
    (if (passedp result)
        (format t message-on-success)
        (format t "~&Test \"~a\" failed with condition:~%\"~a\""
                name (result-condition result)))
    result))

(defun test-results (name)
  "Returns the latests results for a test."
  (gethash name *test-results*))

;; This is ok, but it doesn't have a clear report
(defun run-all-tests (&optional test-list)
  "Run a list of tests, runs all existing tests if no list is provided.
    Returns NIL if they all passed, or the number of failed tests otherwise.
    Also returns the total number of tests as a second value."
  (format t "~&Running tests...")
  (let ((failed 0)
        (total 0)
        (results (make-hash-table)))
    ;; Run each tests
    (loop :for name :in (or test-list (hash-table-keys *test*))
          :for result = (test name ".")
          :for passed = (passedp result)
          :do
             (unless passed
               (incf failed))
             (incf total)
             (setf (gethash name results) result)
          :finally (format t "~&Done [~d/~d] tests passed.~%" (- total failed) total)
                   (force-output))
    ;; Save the test results
    (push *test-results* *test-results-history*)
    (setf *test-results* results)
    ;; Return
    (values (if (zerop failed)
                nil
                failed)
            total)))

(defmacro is (&body body)
  "Macro that signals an error when its body evaluate to nil"
  `(unless (progn ,@body)
     (error "Expression is falsy: ~%~S" '(progn ,@body))))
