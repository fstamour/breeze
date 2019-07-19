
(defpackage #:breeze.worker
  (:use :cl #:alexandria #:anaphora)
  (:export
   ;; class
   #:worker
   ;; accessors
   #:worker-input-channel
   #:worker-output-channel
   #:worker-interval
   #:worker-task
   ;; methods
   #:worker-receive-all-messages
   #:worker-report
   #:worker-process-messages
   #:worker-loop
   #:worker-start
   #:worker-stop
   #:worker-alive-p
   #:worker-ensure-alive
   #:worker-send))

(in-package #:breeze.worker)

(defclass worker ()
  ((input-channel
    :accessor worker-input-channel
    :initarg :input-channel
    :initform (make-instance 'chanl:unbounded-channel))
   (output-channel
    :accessor worker-output-channel
    :initarg :output-channel
    :initform nil)
   (interval
    :accessor worker-interval
    :initarg :interval
    :initform 1
    :type 'positive-real
    :documentation "The time between each iteration worker's loop.
Accepts positive reals (see cl:sleep function).")
   (task
    :accessor worker-task
    ;; :initarg :task
    :initform nil))
  (:documentation "Workers are threads that process messages send via a channel."))


(defgeneric worker-receive-all-messages (worker)
  (:documentation "Get all the messages from the input channel.")
  (:method ((worker worker))
    (loop :for el = (chanl:recv (worker-input-channel worker) :blockp nil)
          :while el
          :collect el)))

(defgeneric worker-report (worker level control-string &rest format-arguments)
  (:documentation "Log a string."))

(defgeneric worker-process-messages (worker message-list)
  (:documentation "Process a batch of messages."))

(defgeneric worker-loop (worker)
  (:documentation "The function that is run in a thread by worker-start"))

(defgeneric worker-start (worker)
  (:documentation "Start the worker's task")
  (:method ((worker worker))
    (setf (worker-task worker) (chanl:pcall
                                #'(lambda () (worker-loop worker))
                                ;; TODO :name "test-runner"
                                ))))

(defgeneric worker-stop (worker)
  (:documentation "Stop the worker's task"))

(defgeneric worker-alive-p (worker)
  (:documentation "Check if the worker's task is running.")
  (:method ((worker worker))
    (and (worker-task worker)
         (eq :alive (chanl:task-status (worker-task worker))))))

(defgeneric worker-ensure-alive (worker)
  (:documentation "Start a worker's task if it's not already running.")
  (:method ((worker worker))
    (unless (worker-alive-p worker)
      (worker-start worker))))

(defgeneric worker-send (worker message)
  (:documentation "Send a message to the worker.")
  (:method ((worker worker) message)
    (chanl:send (worker-input-channel worker) message)))


