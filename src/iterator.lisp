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
           #:reset
           #:copy-vector)
  ;; Classes and Constructors
  (:export #:iterator
           #:proxy-iterator-mixin
           #:selector
           #:make-selector
           #:vector-iterator
           #:nested-vector-iterator
           #:make-vector-iterator
           #:make-nested-vector-iterator
           #:recursive-iterator
           #:make-recursive-iterator
           #:concat-iterator
           #:make-concat-iterator)
  ;; Accessors
  (:export
   #:vec
   #:pos)
  ;; Functions for nested-vector-iterator
  (:export
   #:vectors
   #:positions
   #:push-vector
   #:pop-vector
   #:value-at-depth
   #:parent-value
   #:root-value)
  ;; Other utility functions
  (:export
   #:firstp
   #:lastp
   #:before-last-p
   #:depth
   #:collect
   #:map-iterator))

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

=== a few weeks later ===

I could also define an iterator that uses another one and skip values. So the "base" iterators are kept simple.

|#


;;; Interface (generics)

(defgeneric donep (iterator)
  (:documentation "Check if there's any values left to iterate over."))

(defgeneric next (iterator &key &allow-other-keys)
  (:documentation "Advance the iterator. Might return a whole new iterator, you better
save this function's return value."))

(defgeneric value (iterator)
  (:documentation "Get the current value of the iterator."))

(defgeneric reset (iterator)
  (:documentation "Move the iterator to the beginning."))

(defgeneric copy-iterator (iterator)
  (:documentation "Copy an iterator."))


(defclass iterator () ()
  (:documentation "An abstract iterator."))


;;; vector-iterator

(defclass vector-iterator (iterator)
  ((vector
    :initform (error "Must provide a vector to iterate over.")
    :initarg :vector
    :accessor vec
    :documentation "The vector being iterated over.")
   (position
    :initform 0
    :initarg :position
    :accessor pos
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
  (not (< -1 (pos iterator) (length (vec iterator)))))

(defmethod next ((iterator vector-iterator) &key)
  (incf (pos iterator)))

(defmethod value ((iterator vector-iterator))
  (when (donep iterator) (error "No more values in this iterator. (pos: ~s)"
                                (pos iterator)))
  (aref (vec iterator) (pos iterator)))

;; TODO test
(defmethod reset ((iterator vector-iterator))
  (setf (pos iterator) 0))

;; TODO test
(defmethod copy-iterator ((iterator vector-iterator))
  (make-vector-iterator (vec iterator) :position (pos iterator)))


;;; Other methods on vector-iterator

(defmethod firstp ((iterator vector-iterator))
  "Is the current value the first one in the vector?"
  (zerop (pos iterator)))

(defmethod lastp ((iterator vector-iterator))
  "Is the current value the last one in the vector?"
  (= (pos iterator) (1- (length (vec iterator)))))

(defmethod before-last-p ((iterator vector-iterator))
  "Is the current value the penultimate one in the vector?"
  (= (pos iterator) (- (length (vec iterator)) 2)))


;;; Iterators that encapsulate other iterators

(defclass proxy-iterator-mixin (iterator)
  ((child-iterator
    :initform nil
    :initarg :iterator
    :accessor child-iterator
    :documentation "Child iterator.")))

(defmethod donep ((iterator proxy-iterator-mixin))
  (donep (child-iterator iterator)))

(defmethod next ((iterator proxy-iterator-mixin) &key &allow-other-keys)
  (next (child-iterator iterator)))

(defmethod value ((iterator proxy-iterator-mixin))
  (value (child-iterator iterator)))

;; TODO test
(defmethod reset ((iterator proxy-iterator-mixin))
  (reset (child-iterator iterator)))

(defmethod leaf-iterator (iterator) iterator)

(defmethod leaf-iterator ((iterator proxy-iterator-mixin))
  (leaf-iterator (child-iterator iterator)))


;;; Selector - iterator that skip values

(defclass selector (proxy-iterator-mixin)
  ((filter-in
    :initform nil
    :initarg :filter-in
    :accessor filter-in
    :documentation "Predicate to remove certain values. (If it returns nil for a value, that value is skipped.")))

(defun make-selector (child-iterator filter-in
                      &key
                        dont-skip-p
                        apply-filter-to-iterator-p)
  "Construct a selector from CHILD-ITERATOR.

If DONT-SKIP-P is non-nil, the first elements won't be skipped
automatically even if the SKIP-VALUE-P predicate would be true for
those values.

If APPLY-FILTER-TO-ITERATOR-P is non-nil, the predicate FILTER-IN will be applied to the CHILD-ITERATOR instead of the its current value. This can be used for example to skip the last value, by accessing the iterator's state."
  (let ((iterator (make-instance
                   'selector
                   :iterator child-iterator
                   :filter-in (if apply-filter-to-iterator-p
                                  filter-in
                                  (lambda (iterator)
                                    (funcall filter-in (value iterator)))))))
    (unless dont-skip-p
      (when (skipp iterator)
        (next iterator)))
    iterator))

(defmethod skipp ((iterator selector))
  "Check whether to skip the current value or not"
  (let ((predicate (filter-in iterator)))
    (and predicate (not (donep iterator))
         (not (funcall predicate (leaf-iterator iterator))))))

(defmethod next ((iterator selector) &key dont-skip-p)
  (if dont-skip-p
      (next (child-iterator iterator))
      (loop
        :do (next (child-iterator iterator))
        :while (and (not (donep iterator))
                    (skipp iterator)))))


;;; iterator for nested vectors

(defclass nested-vector-iterator (iterator)
  ((vectors
    :initform (make-array '(0)
                          :element-type 'vector
                          :adjustable t
                          :fill-pointer t)
    :initarg :vectors
    :accessor vectors)
   (depth
    :initform -1
    :initarg :depth
    :accessor depth)
   (positions
    :initform (make-array '(1)
                          :element-type '(integer 0)
                          :adjustable t
                          :initial-element 0
                          :fill-pointer 0)
    :initarg :positions
    :accessor positions))
  (:documentation "An iterator for nested vectors."))

(defun make-nested-vector-iterator (vector)
  "Create a new depth-first iterator on VECTOR."
  (check-type vector vector)
  (push-vector (make-instance 'nested-vector-iterator) vector))

(defmethod vec ((iterator nested-vector-iterator))
  (aref (vectors iterator) (depth iterator)))

(defmethod pos ((iterator nested-vector-iterator))
  (aref (positions iterator) (depth iterator)))

(defmethod (setf pos) (new-position (iterator nested-vector-iterator))
  (setf (aref (positions iterator) (depth iterator)) new-position))

;; same implementation as vector-iterator's
(defmethod donep ((iterator nested-vector-iterator))
  (not (< -1 (pos iterator) (length (vec iterator)))))

;; same implementation as vector-iterator's
(defmethod next ((iterator nested-vector-iterator) &key)
  (incf (pos iterator)))

;; same implementation as vector-iterator's
(defmethod value ((iterator nested-vector-iterator))
  (when (donep iterator)
    (error "No more values in this iterator. (depth: ~s pos: ~s)"
           (depth iterator)
           (pos iterator)))
  (aref (vec iterator) (pos iterator)))

;; TODO test
(defmethod reset ((iterator nested-vector-iterator))
  (setf (depth iterator) 0
        (pos iterator) 0
        (fill-pointer (vectors iterator)) 1
        (fill-pointer (positions iterator)) 1))

;; TODO test
(defmethod copy-iterator ((iterator nested-vector-iterator))
  (flet ((copy-vec (vec)
            (make-array (length vec)
                     :element-type (array-element-type vec)
                     :adjustable t
                     :fill-pointer (fill-pointer vec)
                     :initial-contents vec)))
    (make-instance
     'nested-vector-iterator
     :vectors (copy-vec (vectors iterator))
     :positions (copy-vec (positions iterator))
     :depth (depth iterator))))

(defmethod push-vector ((iterator nested-vector-iterator)
                        vector &key (position 0))
  (vector-push-extend vector (vectors iterator))
  (vector-push-extend position (positions iterator))
  (incf (depth iterator))
  iterator)

(defmethod pop-vector ((iterator nested-vector-iterator))
  (decf (fill-pointer (vectors iterator)))
  (decf (fill-pointer (positions iterator)))
  (decf (depth iterator))
  iterator)

;; TODO add tests
(defmethod value-at-depth ((iterator nested-vector-iterator) depth)
  (let ((pos (aref (positions iterator) depth))
        (vec (aref (vectors iterator) depth)))
    (aref vec pos)))

;; TODO add tests
(defmethod parent-value ((iterator nested-vector-iterator))
  (when (plusp (depth iterator))
    (value-at-depth iterator (1- (depth iterator)))))

;; TODO add tests
(defmethod root-value ((iterator nested-vector-iterator))
  (value-at-depth iterator 0))


;;; Depth-first iterator

(defclass recursive-iterator (nested-vector-iterator)
  ((recurse-into
    :initform nil
    :initarg :recurse-into
    :accessor recurse-into
    :documentation "Callback to control which value to recurse into.")
   (order
    :initform nil
    :initarg :order
    :accessor order
    :documentation "Defines the \"tree-traversal\" order. Valid values are
:subtree-only (the default) or :root-then-subtree. Other possible
orders exists, but they're not needed by breeze for now."))
  (:documentation "Iterator for nested vectors that automatically recurse into subtrees."))

(defun make-recursive-iterator (vector recurse-into &key apply-recurse-into-to-iterator-p order)
  (push-vector
   (make-instance 'recursive-iterator
                  :recurse-into (if apply-recurse-into-to-iterator-p
                                    recurse-into
                                    (lambda (iterator)
                                      (funcall recurse-into (value iterator))))
                  :order (ecase order
                           (:root-then-subtree order)
                           ;; default
                           ((nil :subtree-only) :subtree-only)))

   vector))


(defmethod maybe-dig-in ((iterator recursive-iterator))
  (unless (donep iterator)
    (let ((value-to-dig-in (funcall (recurse-into iterator) iterator)))
      (when value-to-dig-in
        (push-vector iterator
                     (if (eq t value-to-dig-in)
                         (value iterator)
                         value-to-dig-in))
        t))))

(defmethod maybe-dig-out ((iterator recursive-iterator))
  ;; TODO this docstring is not exactly right, it's still written as
  ;; if iterator was like a linked list (it used to be).
  "If ITERATOR is done and has a parent, return the next
parent (i.e. the parent might also be done, in which case the parent's
parents is returned, and so on and so on). Otherwise, just return
ITERATOR unchanged."
  (loop
    :while (and (donep iterator) (< 0 (depth iterator)))
    :do (pop-vector iterator) (next iterator :dont-recurse-p t)))

(defmethod next ((iterator recursive-iterator) &key dont-recurse-p)
  (ecase (order iterator)
    (:root-then-subtree
     ;; if the current node is "recursable", then we recurse into it
     (unless (unless dont-recurse-p
               (maybe-dig-in iterator))
       (call-next-method)
       (maybe-dig-out iterator)))
    ;; default
    ((nil :subtree-only)
     (call-next-method)
     (maybe-dig-out iterator)
     (unless dont-recurse-p
       (maybe-dig-in iterator)))))


;;; Concat iterator
;;
;; TODO maybe find a better name
;; TODO add documentation
;; TODO add tests

(defclass concat-iterator (vector-iterator) ())

(defun make-concat-iterator (iterators)
  (make-instance 'concat-iterator :vector (coerce iterators 'vector)))

(defmethod child-iterator ((iterator concat-iterator))
  (aref (vec iterator) (pos iterator)))

(defmethod next ((iterator concat-iterator) &key &allow-other-keys)
  (loop :do (next (child-iterator iterator))
            (when (donep (child-iterator iterator))
              (incf (pos iterator)))
        :until (or (donep iterator)
                   (not (donep (child-iterator iterator))))))

(defmethod value ((iterator concat-iterator))
  (value (call-next-method iterator)))

(defmethod leaf-iterator ((iterator concat-iterator))
  (leaf-iterator (value iterator)))



(defun collect (iterator &rest rest &key (limit) &allow-other-keys)
  (let ((next-extra-args (alexandria:remove-from-plist rest :limit)))
    (loop
      with i = 0
      while (or (null limit) (<= (incf i) limit))
      until (donep iterator)
      collect (value iterator)
      do (apply #'next iterator next-extra-args))))

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
