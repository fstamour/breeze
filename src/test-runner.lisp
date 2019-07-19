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

;; TODO Get rid of this global
(defvar *stop-test-runner* nil
  "If true, the test-runner-thread will stop on the next iteration.")

(defclass test-runner (worker) ())

(defvar *test-runner* (make-instance 'test-runner :interval 0.25))

(defmethod worker-report ((test-runner test-runner) level control-string &rest format-arguments)
  (declare (ignore test-runner level))
  (when nil ;; don't log anything anywhere for now
    (fresh-line)
    (apply #'format t control-string format-arguments)
    (fresh-line)
    (force-output)))

;; (defmethod worker-process-messages ((test-runner test-runner) message-list)
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
    (run-all-tests (hash-table-keys tests))
    #+nil
    (format t "~&Would run: ~A~%Would skip: ~A~%"
            (hash-table-keys tests)
            skipped)
    #+nil
    (force-output)))

(defun receive-messages ()
  (worker-receive-all-messages *test-runner*))

(defun test-runner-loop (test-runner)
  (let ((messages ())
        (received-messages-last-iteration-p nil))
    (worker-report test-runner :info "test runner started")
    (loop :until *stop-test-runner*
          :do
             (worker-report test-runner :debug "~%running test runner~%Messages: ~A~%received: ~A"
                            messages received-messages-last-iteration-p)
             (aif (receive-messages)
                  (setf messages (append messages it)
                        received-messages-last-iteration-p t)
                  (setf received-messages-last-iteration-p nil))
             (when (and messages (not received-messages-last-iteration-p))
               (process-messages messages)
               (setf received-messages-last-iteration-p nil
                     messages nil))
             (sleep (worker-interval test-runner)))
    (worker-report test-runner :info "test runner stopped")))

(defmethod worker-loop ((test-runner test-runner))
  (test-runner-loop test-runner))

;; around methods are awesome
(defmethod worker-start :around ((test-runner test-runner))
  "Start the test runner thread"
  (setf *stop-test-runner* nil)
  (call-next-method))

(defmethod worker-stop ((test-runner test-runner))
  "Stop the test runner thread."
  (setf *stop-test-runner* t))

(defun ensure-test-runner ()
  (worker-ensure-alive *test-runner*))

(defun request-to-run-test (test)
  "Take a test name and send it to the test-runner."
  (worker-send *test-runner* (list test (get-universal-time))))

(defun request-to-run-test* (test-list)
  "Take a list of test name and send it to the test-runner."
  (map nil #'request-to-run-test test-list))

;; Bunch of calls used to manually test the test-runner's logic
#|
(start-test-runner)
(stop-test-runner)
(request-to-run-test 'a)
(request-to-run-test* '(a b c)))
(progn
  (request-to-run-test 'a)
  (request-to-run-test* '(a b c)))
|#


