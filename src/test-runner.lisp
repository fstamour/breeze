;;; The test runner is a separate thread that receives request to run test.
;;; We use a thread in order to debounce and group the requests.
;;; TODO Logging (maybe use log4cl)

(defpackage #:breeze.test-runner
  (:use :cl #:alexandria #:anaphora
        #:breeze.worker)
  (:export #:start-test-runner
           #:stop-test-runner
           #:ensure-test-runner
           #:request-to-run-test
           #:request-to-run-test*)
  (:import-from #:breeze.test
                #:run-all-tests))

(in-package #:breeze.test-runner)

(defclass test-runner (worker)
  ((messages :initform ())
   (received-messages-last-iteration-p :initform nil))
  (:documentation "A test runner is a worker that run tests on demand."))

(defvar *test-runner* (make-instance 'test-runner
                                     :interval 0.25
                                     :name "test runner")
  "The main test-runner.")

(defun process-messages (message-list)
  "Take a list of messages (already debounced) and deduplicate and process them."
  (let ((tests (make-hash-table))
        (skipped ()))
    (loop :for message :in message-list
          :do
             (destructuring-bind (test-name timestamp)
                 message
               (declare (ignore timestamp))
               (if (gethash test-name tests)
                   (push message skipped)
                   (setf (gethash test-name tests) t))))
    (run-all-tests (hash-table-keys tests))))

(defun receive-messages ()
  (worker-receive-all-messages *test-runner*))

(defmethod worker-report ((test-runner test-runner) level control-string &rest format-arguments)
  ;; Make the test-worker quiet (do nothing)
  ;; (call-next-method) ;; <- print to standard-output
  )

(defmethod worker-run ((test-runner test-runner))
  (with-slots (messages received-messages-last-iteration-p)
      test-runner
    (worker-report test-runner :debug "~%running test runner~%Messages: ~A~%received: ~A"
                   messages received-messages-last-iteration-p)
    (aif (receive-messages)
         (setf messages (append messages it)
               received-messages-last-iteration-p t)
         (setf received-messages-last-iteration-p nil))
    (when (and messages (not received-messages-last-iteration-p))
      (process-messages messages)
      (setf received-messages-last-iteration-p nil
            messages nil))))

(defun start-test-runner ()
  "Start the test runner"
  (worker-start *test-runner*))

(defun stop-test-runner ()
  "Stop the test runner"
  (worker-stop *test-runner*))

(defun ensure-test-runner ()
  "Start the test runner if it's not already running."
  (worker-ensure-alive *test-runner*))

(defun request-to-run-test (test)
  "Take a test name and send it to the test-runner."
  (worker-send *test-runner* (list test (get-universal-time))))

(defun request-to-run-test* (test-list)
  "Take a list of test name and send it to the test-runner."
  (map nil #'request-to-run-test test-list))

;; Bunch of calls used to manually test the test-runner's logic
#|
(worker-start *test-runner*)
(worker-stop *test-runner*)
(worker-alive-p *test-runner*)

(request-to-run-test 'a)
(request-to-run-test* '(a b c)))
(progn
  (request-to-run-test 'a)
  (request-to-run-test* '(a b c)))
|#


