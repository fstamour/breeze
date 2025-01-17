#|

Iterator on vectors and nested vectors that:
- takes care of "recursing" into some values
- conditionally skips some values

Because both the syntax trees and the (compiled) patterns are nested
vectors.

Historical note: this started as a struct in pattern.lisp. This is a
generalization of that first iteration (ha!).

|#


;;; breeze.iterator package definition

(defpackage #:breeze.iterator
  (:documentation "Iterators for nested vectors")
  (:use #:cl)
  ;; Generics
  (:export #:donep
           #:next
           #:value
           #:skipp
           #:digp)
  ;; Classes and Constructors
  (:export #:vector-iterator
           #:depth-first-iterator
           #:make-vector-iterator
           #:make-depth-first-iterator)
  ;; Accessors
  (:export
   #:vec
   #:pos
   #:parent)
  ;; Other utility functions
  (:export
   #:firstp
   #:lastp
   #:before-last-p
   #:depth
   #:collect))

(in-package #:breeze.iterator)


;;; Notes on skipping values while iterating

#|

I'm not sure which way to specify which values to skip...

1. put a predicate in a special variable
2. put a predicate in the iterator
3. use a specialized method

A) 1. is what I was already using in the first version of the
iterators (in pattern.lisp, using defstruct)

B) 1. and 2. are pretty flexible

C) 1. means that you cannot easily have different iterators that skip
different things at the same time (you would have to bind the special
variable to a different function before every call that uses the
predicate).

D) 3. means that you need to create a new class for each things you might
want to skip. this might be harder to compose that 1. or 2.

Conclusion:

I think I'll go with "2. put a predicate in the iterator" because of
C) and D)

P.S. By the same logic, I'll put a predicate in the iterator to
control whether to recursively iterate over a value's elements.

|#


;;; Interface (generics)

(defgeneric donep (iterator)
  (:documentation "Check if there's any values left to iterate over."))

(defgeneric next (iterator &key &allow-other-keys)
  (:documentation "Advance the iterator. Might return a whole new iterator, you better
save this function's return value."))

(defgeneric value (iterator)
  (:documentation "Get the current value of the iterator"))

(defgeneric skipp (iterator)
  (:documentation "Check whether to skip the current value or not")
  (:method (iterator) "Default implementation" nil))

(defgeneric digp (iterator)
  (:documentation "Check whether to iterate on the current value's elements or not.")
  (:method (iterator) "Default implementation" nil))


;;; vector-iterator

;; TODO I'm not sure I want to keep the "vector-iterator" class,
;; because everything it can do, the "depth-first-iterator" can
;; do... but this one is definitly simpler... but I would need more
;; tests if I want to keep both.

(defclass vector-iterator ()
  ((vector
    :initform (error "Must provide a vector to iterate over.")
    :initarg :vector
    :accessor vec
    :documentation "The vector being iterated over.")
   (position
    :initform 0
    :initarg :position
    :accessor pos
    :documentation "Current position of the iterator in the vector.")
   (skip-value-p
    :initform nil
    :initarg :skip-value-p
    :accessor skip-value-p
    :documentation "Optional predicate to skip over certain values."))
  (:documentation "An iterator for vectors."))

(defun make-vector-iterator (vector &key
                                      (position 0)
                                      (skip-value-p nil)
                                      dont-skip-p)
  "Construct a vector-iterator for VECTOR.

If DONT-SKIP-P is non-nil, the first elements won't be skipped
automatically even if the SKIP-VALUE-P predicate would be true for
those values."
  (check-type vector vector)
  (let ((iterator
          (make-instance 'vector-iterator
                         :vector vector
                         :position position
                         :skip-value-p skip-value-p)))
    (unless dont-skip-p
      (when (skipp iterator)
        (next iterator)))
    iterator))

(defmethod skipp ((iterator vector-iterator))
  (let ((predicate (skip-value-p iterator)))
    (and predicate (not (donep iterator))
         (funcall predicate iterator))))

(defmethod donep ((iterator vector-iterator))
  (not (< -1 (pos iterator) (length (vec iterator)))))

(defmethod next ((iterator vector-iterator) &key dont-skip-p)
  (if dont-skip-p
      (incf (pos iterator))
      (loop
        :do (incf (pos iterator))
        :while (and (not (donep iterator))
                    (skipp iterator)))))

(defmethod value ((iterator vector-iterator))
  (when (donep iterator) (error "No more values in this iterator."))
  (aref (vec iterator) (pos iterator)))


;;; Other methods on vector-iterator

(defmethod firstp ((iterator vector-iterator))
  "Is the current value the first one in the vector?"
  (zerop (pos iterator)))

(defmethod lastp ((iterator vector-iterator))
  "Is the current value the last one in the vector?"
  (= (pos iterator) (length (vec iterator))))

(defmethod before-last-p ((iterator vector-iterator))
  "Is the current value the penultimate one in the vector?"
  (= (pos iterator) (1- (length (vec iterator)))))


;;; depth-first iterator
;;;
;;; Would "nested" iterator be a better name?

(defclass depth-first-iterator (vector-iterator)
  ((parent
    :initform nil
    :initarg :parent
    :accessor parent)
   (dig-value-p
    :initform nil
    :initarg :dig-value-p
    :accessor dig-value-p
    :documentation "Optional predicate to dig into certain values."))
  (:documentation "A depth-first iterator for nested vectors."))

(defun make-depth-first-iterator (vector &key
                                           (position 0)
                                           (skip-value-p nil)
                                           (dig-value-p nil))
  "Create a new depth-first iterator on VECTOR."
  (check-type vector vector)
  (let ((iterator (maybe-dig (make-instance
                              'depth-first-iterator
                              :vector vector
                              :position position
                              :skip-value-p skip-value-p
                              :dig-value-p dig-value-p))))
    (if (skipp iterator) (next iterator) iterator)))

(defmethod depth ((iterator depth-first-iterator))
  (if (null (parent iterator)) 0 (1+ (depth (parent iterator)))))


;;; Implementing "next" method for "depth-first-iterator"

(defmethod digp ((iterator depth-first-iterator))
  (let ((predicate (dig-value-p iterator)))
    (and predicate
         (funcall predicate iterator))))

(defmethod dig ((iterator depth-first-iterator) x)
  "Create a new iterator on VECTOR, with ITERATOR as parent. Returns the
new iterator."
  (make-instance 'depth-first-iterator
                 :vector (etypecase x
                           (vector x)
                           ;; or just T to specify to iterate on the
                           ;; current value.
                           ((eql t) (value iterator)))
                 :parent iterator
                 :skip-value-p (skip-value-p iterator)
                 :dig-value-p (dig-value-p iterator)))

(defun maybe-dig (iterator)
  "If ITERATOR is not done and the current value needs to be iterated
over, \"push\" a new iterator."
  (if (donep iterator)
      iterator
      ;; dig-value-p is allowed to return a different any vector, not
      ;; just the current value.
      (let ((value-to-dig-into (digp iterator)))
        (if value-to-dig-into
            (maybe-dig (dig iterator value-to-dig-into))
            iterator))))

(defun maybe-dig-out (iterator)
  "If ITERATOR is done and has a parent, return the next
parent (i.e. the parent might also be done, in which case the parent's
parents is returned, and so on and so on). Otherwise, just return
ITERATOR unchanged."
  (check-type iterator depth-first-iterator)
  (if (and (donep iterator) (parent iterator))
      (let ((parent (parent iterator)))
        ;; Advance the position
        (incf (pos parent))
        ;; return the parent
        (maybe-dig-out parent))
      iterator))

(defmethod next ((iterator depth-first-iterator)
                 &key dont-skip-p dont-dig-p)
  ;; TODO handle dont-skip-p
  ;; TODO handle dont-dig-p
  (flet ((%next (iterator)
           (incf (pos iterator))
           (maybe-dig (maybe-dig-out iterator))))
    (loop :for new-iterator = (%next iterator)
            :then (%next new-iterator)
          :while (skipp new-iterator)
          :finally (return new-iterator))))




;; TODO This doesn't work well with the current implementation of the
;; "depth-first" iterators.
(defun collect (iterator &key (limit))
  (if limit
      (loop
        repeat limit
        until (donep iterator)
        collect (value iterator)
        do (next iterator))
      (loop
        until (donep iterator)
        collect (value iterator)
        do (next iterator))))
