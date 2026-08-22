#|

This file defines the abstract iterator class, the generic interface
all iterators implement, and a few utilities that work on any iterator
(`collect', `map-iterator', `iterator-value').

|#

(in-package #:breeze.iterator)


;;; Interface (generics)

(defgeneric donep (iterator)
  (:documentation "Check if there's any values left to iterate over."))

(defgeneric next (iterator)
  (:documentation "Advance the iterator. Might return a whole new iterator, you better
save this function's return value."))

(defgeneric value (iterator)
  (:documentation "Get the current value of the iterator."))

(defgeneric reset (iterator)
  (:documentation "Move the iterator to the beginning."))

(defgeneric copy-iterator (iterator &optional target)
  (:documentation "Copy an iterator."))

(defgeneric current-position (iterator)
  (:documentation "Current position of the iterator"))


(defclass iterator () ()
  (:documentation "An abstract iterator."))

(defun iteratorp (x)
  (typep x 'iterator))


;;; Generic utilities

(defmethod next-iterator ((iterator iterator))
  "Get a copy of ITERATOR moved to the next position."
  ;; TODO perf: this copies the iterator regardless if there is a
  ;; next element or not.
  (let ((it (copy-iterator iterator)))
    (next it)
    (unless (donep it) it)))

(defmethod previous-iterator ((iterator iterator))
  "Get a copy of ITERATOR and move it backward to the previous
position (if it exists)."
  ;; TODO perf: this copies the iterator regardless if there is a
  ;; previous element or not.
  (let ((it (copy-iterator iterator)))
    (and (go-backward it 1) it)))

(defun collect (iterator &key (limit))
  (loop
    with i = 0
    while (or (null limit) (<= (incf i) limit))
    until (donep iterator)
    collect (value iterator)
    do (next iterator)))

(defun map-iterator (fn iterator &key (limit))
  (if limit
      (loop
        repeat limit
        until (donep iterator)
        do (funcall fn (value iterator))
        do (next iterator))
      (loop
        until (donep iterator)
        do (funcall fn (value iterator))
        do (next iterator))))

;; TODO rename to skip-forward
;; TODO add a skip-backward
(declaim (inline skip))
(defun skip ($input skipp)
  (when skipp
    (loop :while (and
                  (not (donep $input))
                  (funcall skipp $input))
          :do (next $input))))


;;; iterator-value: comparison helper

(define-class iterator-value (:positional-args (value))
  ((value :initarg :value :accessor value))
  (:documentation "An object used to compare a value against an iterator's current value.

Example:

(eqv \"x\" (make-vector-iterator #(\"x\"))) => nil
(eqv (iterator-value \"x\") (make-vector-iterator #(\"x\"))) => t
"))

(defmethod eqv ((a iterator-value) (b iterator))
  (unless (donep b)
    (eqv (value a) (value b))))

(defmethod eqv ((a iterator) (b iterator-value))
  (unless (donep a)
    (eqv (value a) (value b))))
