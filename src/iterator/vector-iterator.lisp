(in-package #:breeze.iterator)

(defclass vector-iterator (iterator)
  ((vector
    :initform (error "Must provide a vector to iterate over.")
    :initarg :vector
    :type vector
    :documentation "The vector being iterated over.")
   (position
    :initform 0
    :initarg :position
    :type integer
    :accessor current-position
    :documentation "Current position of the iterator in the vector."))
  (:documentation "An iterator for vectors."))

(defun make-vector-iterator (vector &key (position 0))
  "Construct a vector-iterator for VECTOR."
  (check-type vector vector)
  (check-type position (integer 0))
  (make-instance 'vector-iterator
                 :vector vector
                 :position position))

(defmethod donep ((iterator vector-iterator))
  (with-slots (position vector) iterator
    (not (< -1 position (length vector)))))

(defmethod next ((iterator vector-iterator))
  (with-slots (position) iterator
    (incf position)))

(defmethod value ((iterator vector-iterator))
  (with-slots (position vector) iterator
    (aref vector position)))

;; TODO test
(defmethod reset ((iterator vector-iterator))
  (with-slots (position) iterator
    (setf position 0)))

;; TODO test
(defmethod copy-iterator ((iterator vector-iterator) &optional target)
  (with-slots (position vector) iterator
    (cond
      (target
       (setf (slot-value target 'vector) vector
             (slot-value target 'position) position)
       target)
      (t
       (make-vector-iterator vector :position position)))))

(defmethod print-object ((vector-iterator vector-iterator) stream)
  (print-unreadable-object
      (vector-iterator stream :type t :identity nil)
    (with-slots (position) vector-iterator
      (format stream "~s" position))))


;;; Other methods on vector-iterator

(defmethod eqv ((a vector-iterator) (b vector-iterator))
  (with-slots ((pos-a position) (vec-a vector)) a
    (with-slots ((pos-b position) (vec-b vector)) b
      (and (eqv vec-a vec-b)
           (eqv pos-a pos-b)))))

(defmethod firstp ((iterator vector-iterator))
  "Is the current value the first one in the vector?"
  (with-slots (position) iterator
    (zerop position)))

(defmethod lastp ((iterator vector-iterator))
  "Is the current value the last one in the vector?"
  (with-slots (position vector) iterator
    (= position (1- (length vector)))))

(defmethod before-last-p ((iterator vector-iterator))
  "Is the current value the penultimate one in the vector?"
  (with-slots (position vector) iterator
    (= position (- (length vector) 2))))
