
(defpackage #:breeze.channel
  (:documentation "A simple channel implementation.")
  (:use #:cl)
  (:export #:make-channel
           #:channelp
           #:channel
           #:send
           #:receive
           #:emptyp))

(in-package #:breeze.channel)

(defclass channel ()
  ((queue
    :initform (cons nil nil)
    :documentation "The content of the channel's queue.")
   (lock
    :initform (bt2:make-lock)
    :documentation "Mutex lock")
   (datap
    :initform (bt2:make-semaphore)
    :documentation "Used to signal available data for the readers."))
  (:documentation "A minimal unbounded, queue-backed (FIFO) channel."))

(defun make-channel ()
  "Make a new unbounded channel."
  (make-instance 'channel))

(defun channelp (x)
  (typep x 'channel))

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (format stream "~a" (length (car (slot-value channel 'queue))))))

(declaim (inline empty-queue-p))
(defun empty-queue-p (q)
  (and (null (car q)) (null (cdr q))))

(declaim (inline enqueue))
(defun enqueue (q v)
  (let ((end (list v)))
    (if (empty-queue-p q)
        (setf (car q) end)
        (setf (cddr q) end))
    (setf (cdr q) end)))

(declaim (inline dequeue))
(defun dequeue (q)
  (prog1 (pop (car q))
    (unless (car q)
      (setf (cdr q) nil))))



(defun send (c v)
  "Send the value V into the channel C, doesn't block (much)."
  (with-slots (queue lock datap) c
   (bt2:with-lock-held (lock #| TODO :timeout timeout |#)
     (enqueue queue v))
    (bt2:signal-semaphore datap)))

(defun receive (c)
  "Receive a value from the channel C, blocks if there's not message
available."
  (with-slots (queue lock datap) c
    (loop
      (bt2:wait-on-semaphore datap #| TODO :timeout timeout |#)
      (bt2:with-lock-held (lock)
        (unless (empty-queue-p queue)
          (return (dequeue queue)))))))

(defun emptyp (c)
  (with-slots (queue lock datap) c
   (bt2:with-lock-held (lock)
     (empty-queue-p queue))))
