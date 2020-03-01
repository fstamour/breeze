
(defpackage #:breeze.worker
  (:documentation "This package exports a class named \"worker\" and different methods to manipulate objects of that class.")
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
   #:worker-run
   #:worker-start
   #:worker-stop
   #:worker-alive-p
   #:worker-ensure-alive
   #:worker-send))

(in-package #:breeze.worker)

(defclass worker ()
  ((name
    :accessor worker-name
    :initarg :name
    :initform "worker"
    :documentation "The name of this worker (for simpler identification).")
   (input-channel
    :accessor worker-input-channel
    :initarg :input-channel
    :initform (make-instance 'chanl:unbounded-channel)
    :documentation "Channel used to send data to the task.")
   (output-channel
    :accessor worker-output-channel
    :initarg :output-channel
    :initform nil
    :documentation "Channel to get data back from the task.")
   (control-channel
     :accessor worker-control-channel
     :initarg :control-channel
     :initform (make-instance 'chanl:unbounded-channel)
     :documentation "Channel to ask to stop the task.")
   (interval
    :accessor worker-interval
    :initarg :interval
    :initform 1
    :type 'positive-real
    :documentation "The time between each iteration of the worker's loop.
Accepts positive reals (see cl:sleep function).")
   (control-interval
    :accessor worker-control-interval
    :initarg :control-interval
    :initform 0.1
    :type 'positive-real
    :documentation "The time between each worker's contol loop
Accepts positive reals (see cl:sleep function).")
   (last-iteration-time
    :accessor worker-last-iteration-time
    :initform nil
    :type 'positive-real
    :documentation "Time that the the worker was called last.")
   (task
    :accessor worker-task
    :initform nil
    :documentation "The function called by the control loop, it is set when calling worker-start."))
  (:documentation "Workers are threads that process messages send via a channels.
There are 3 channels:
* an input channel used to send data to the task
* an ouput channel used to get data back from the task
* a control channel used to stop the task

To use the worker, you need to subclass the worker class and specialize the \"worker-run\" method.
"))

(defgeneric worker-input-channel (worker)
  (:documentation "Get the channel used to send data to the task."))

(defgeneric worker-output-channel (worker)
  (:documentation "Get the channel used to get data from the task."))

(defgeneric worker-interval (worker)
  (:documentation "Get the time between each iterations of the worker's loop."))

(defgeneric worker-receive-all-messages (worker)
  (:documentation "Get all the messages from the input channel.")
  (:method ((worker worker))
    (loop :for el = (chanl:recv (worker-input-channel worker) :blockp nil)
          :while el
          :collect el)))

(defgeneric worker-report (worker level control-string &rest format-arguments)
  (:documentation "Log a string (to the standard output).")
  (:method ((worker worker) level control-string &rest format-arguments)
    (fresh-line)
    (apply #'format t control-string format-arguments)
    (fresh-line)
    (force-output)))

(defgeneric worker-process-messages (worker message-list)
  (:documentation "Process a batch of messages."))

(defun worker-set-last-iteration-time (worker)
  (setf (worker-last-iteration-time worker) (/ (get-internal-real-time)
                                               internal-time-units-per-second)))

(defgeneric worker-run (worker)
  (:documentation "The function that is called periodically.")
  (:method ((worker worker))
    (worker-report worker :debug "running ~a"
                   (worker-name worker))))

(defgeneric worker-control-loop (worker)
  (:documentation "The function that is run in a thread by worker-start")
  (:method ((worker worker))
    (worker-report worker :info "~a started"
                   (worker-name worker))
    (loop :for message = (chanl:recv (worker-control-channel worker) :blockp nil)
          :until (eq message :stop)
          :do
             (worker-report worker :debug "running ~a's control loop"
                            (worker-name worker))
             (let ((current-time (/ (get-internal-real-time)
                                    internal-time-units-per-second))
                   (last-time (worker-last-iteration-time worker)))
               (when (or (null last-time)
                         (> (- current-time last-time)
                            (worker-interval worker)))
                 (worker-set-last-iteration-time worker)
                 (worker-run worker)))
             (sleep (worker-control-interval worker))) ;; TODO This should take into account the worker-interval
    (worker-report worker :info "~a stopped"
                   (worker-name worker))))

(defgeneric worker-start (worker)
  (:documentation "Start the worker's task")
  (:method ((worker worker))
    (setf (worker-task worker) (chanl:pcall
                                #'(lambda () (worker-control-loop worker))
                                :name (worker-name worker)))))

(defgeneric worker-stop (worker)
  (:documentation "Stop the worker's task")
  (:method ((worker worker))
    (chanl:send (worker-control-channel worker) :stop)))

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



#|
(defparameter *test-worker*
  (make-instance 'worker
                 :name "test worker"))

(worker-start *test-worker*)
(worker-stop *test-worker*)
|#

