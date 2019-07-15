;;; The test runner is a separate thread that receives request to run test.
;;; We use a thread in order to debounce and group the requests.
;;; TODO Logging (maybe use log4cl)

(defpackage #:breeze.test-runner
  (:use :cl #:alexandria #:anaphora)
  (:export #:start-test-runner
           #:stop-test-runner
           #:ensure-test-runner
           #:request-to-run-test
           #:request-to-run-test*)
  (:import-from #:breeze.test
                #:run-all-tests))

(in-package #:breeze.test-runner)

(defvar *test-runner-channel* (make-instance 'chanl:unbounded-channel)
  "The channel used to send messages to the test-runner.")

(defvar *stop-test-runner* nil
  "If true, the test-runner-thread will stop on the next iteration.")

(defvar *repeat-interval* 0.25
  "The time between each iteration of the test-runner's main loop.
Accepts positive reals (see cl:sleep function).")

(defvar *test-runner-task* nil
  "The handler to the test runner thread. (it's a chanl:task object).")

(defun receive-messages ()
  "Get all the messages from the *test-runner-channel* channel. Returns a list."
  (loop :for el = (chanl:recv *test-runner-channel* :blockp nil)
        :while el
        :collect el))

(defun log-message (control-string &rest format-arguments)
  (when nil ;; don't log anything anywhere for now
    (fresh-line)
    (apply #'format t control-string format-arguments)
    (fresh-line)
    (force-output)))

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

(defun test-runner-loop ()
  (let ((messages ())
        (received-messages-last-iteration-p nil))
    (log-message "test runner started")
    (loop :until *stop-test-runner*
          :do
             (log-message "~%running test runner~%Messages: ~A~%received: ~A"
                          messages received-messages-last-iteration-p)
             (aif (receive-messages)
                  (setf messages (append messages it)
                        received-messages-last-iteration-p t)
                  (setf received-messages-last-iteration-p nil))
             (when (and messages (not received-messages-last-iteration-p))
               (process-messages messages)
               (setf received-messages-last-iteration-p nil
                     messages nil))
             (sleep *repeat-interval*))
    (log-message "test runner stopped")))

(defun start-test-runner ()
  "Start the test runner thread"
  (setf *stop-test-runner* nil
        *test-runner-task* (chanl:pcall 'test-runner-loop :name "test-runner")))

(defun stop-test-runner ()
  "Stop the test runner thread."
  (setf *stop-test-runner* t))

(defun test-runner-alive-p ()
  "Check if test-runner is running."
  (and *test-runner-task*
       (eq :alive (chanl:task-status *test-runner-task*))))

(defun ensure-test-runner ()
  (unless (test-runner-alive-p)
    (start-test-runner)))

(defun request-to-run-test (test)
  "Take a test name and send it to the test-runner."
  (chanl:send *test-runner-channel* (list test (get-universal-time))))

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


