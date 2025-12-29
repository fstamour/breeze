(defpackage #:breeze.queue
  (:documentation "The simplest implementation of a queue: a cons whose car points to the
beginning of a proper list and the cdr points to the last car of that
list.")
  (:use #:cl)
  (:export #:make-queue
           #:empty-queue-p
           #:enqueue
           #:dequeue))

(in-package #:breeze.queue)

(declaim (inline make-queue))
(defun make-queue ()
  "Create a new empty queue."
  (cons nil nil))

(declaim (inline empty-queue-p))
(defun empty-queue-p (q)
  "Return true if Q is an empty queue (any cons with both car and cdr
pointing to nil)."
  (and (null (car q)) (null (cdr q))))

(declaim (inline enqueue))
(defun enqueue (q v)
  "Append the value V to the queue Q."
  (let ((end (list v)))
    (if (empty-queue-p q)
        (setf (car q) end)
        (setf (cddr q) end))
    (setf (cdr q) end)))

(declaim (inline dequeue))
(defun dequeue (q)
  "Pop the value from the start of the queue Q."
  (prog1 (pop (car q))
    (unless (car q)
      (setf (cdr q) nil))))
