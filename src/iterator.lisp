#|

Iterator on vectors and nested vectors (tree) that:
- helps with "recursing" into some values
- conditionally skips some values

Because both the syntax trees and the (compiled) patterns are nested
vectors.

Historical note: this started as a struct in pattern.lisp. This is a
generalization of that first iteration (ha!).

|#


;;; breeze.iterator package definition

(uiop:define-package #:breeze.iterator
  (:documentation "Iterators for nested vectors")
  (:use #:cl)
  ;; Generics
  (:export #:donep
           #:next
           #:value
           #:reset
           #:copy-iterator
           #:current-position)
  ;; Classes and Constructors
  (:export #:iterator
           #:proxy-iterator-mixin
           #:selector
           #:make-selector
           #:vector-iterator
           #:tree-iterator
           #:make-vector-iterator
           #:make-tree-iterator
           #:concat-iterator
           #:make-concat-iterator)
  ;; Accessors
  (:export
   #:subtree
   #:pos)
  ;; Functions for tree-iterator
  (:export
   #:subtrees
   #:positions
   #:push-subtree
   #:pop-subtree
   #:go-down
   #:go-up
   #:subtree-at-depth
   #:root-subtree
   #:goto-root
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

(defgeneric copy-iterator (iterator &optional target)
  (:documentation "Copy an iterator."))

(defgeneric current-position (iterator)
  (:documentation "Current position of the iterator"))


(defclass iterator () ()
  (:documentation "An abstract iterator."))


;;; vector-iterator

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

(defmethod next ((iterator vector-iterator) &key)
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


;;; Other methods on vector-iterator

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


;;; iterator for trees (nested vectors)

(defclass tree-iterator (iterator)
  ((subtrees
    :initform (make-array '(0)
                          :element-type 'vector
                          :adjustable t
                          :fill-pointer t)
    :initarg :subtrees
    :documentation "A vector containing the current subtree being at each level

For example, the first element is the root of the tree, and the last
active element is current subtree).")
   (depth
    :initform 0
    :initarg :depth
    :type fixnum
    :documentation "Current depth. Used to get the current subtree from `subtrees' and the
current position in the subtree from `positions'.")
   (positions
    :initform (make-array '(1)
                          :element-type 'fixnum
                          :adjustable t
                          :initial-element 0
                          :fill-pointer 0)
    :initarg :positions
    :type (vector fixnum)
    :documentation "A vector of positions, each one representing the position at different
depth of the tree."))
  (:documentation "An iterator for nested subtrees."))

(defmethod print-object ((iterator tree-iterator) stream)
  (print-unreadable-object
      (iterator stream :type t :identity t)
    (with-slots (depth positions) iterator
        (format stream "depth: ~s pos: ~s" depth positions))))

(defmethod initialize-instance
    ((iterator tree-iterator)
     &rest initargs
     &key root &allow-other-keys)
  (apply #'shared-initialize iterator t initargs)
  ;; root is nil when the tree-iterator is being copied
  (when root (push-subtree iterator root)))

(defun make-tree-iterator (root)
  "Create a new iterator on ROOT."
  (make-instance 'tree-iterator :root root))

(defmethod children ((iterator tree-iterator))
  (let ((value (value iterator)))
    (when (vectorp value) value)))

(declaim (inline subtree))
(defun subtree (iterator)
  #++ (declare (optimize (speed 3)))
  #|
  unable to
    optimize
  because:
    Upgraded element type of array is not known at compile time.
  |#
  (with-slots (subtrees depth) iterator
      (aref subtrees depth)))

(declaim (inline pos))
(defun pos (iterator)
  #++ (declare (optimize (speed 3)))
  #|
  unable to
    optimize
  because:
    Upgraded element type of array is not known at compile time.
  |#
  (with-slots (positions depth) iterator
    (aref positions depth)))

(defun (setf pos) (new-position iterator)
  (with-slots (positions depth) iterator
    (setf (aref positions depth) new-position)))

(defmethod current-position ((iterator tree-iterator))
  (pos iterator))

(defmethod (setf current-position) (new-position (iterator tree-iterator))
  (setf (pos iterator) new-position))

(declaim (inline current-depth-done-p))
(defun current-depth-done-p (iterator)
  (or
   ;; too deep
   (with-slots (depth subtrees) iterator
     (<= (length subtrees) depth))
   (if (typep (subtree iterator) 'sequence)
       (not (< -1 (pos iterator) (length (subtree iterator))))
       ;; if the subtree is not a vector, then 0 is the only valid
       ;; position
       (plusp (pos iterator)))))

(defmethod donep ((iterator tree-iterator))
  (with-slots (depth) iterator
    (and (zerop depth)
         (current-depth-done-p iterator))))

(defmethod next ((iterator tree-iterator) &key)
  (incf (pos iterator)))

(defmethod value ((iterator tree-iterator))
  (aref (subtree iterator) (pos iterator)))

;; TODO test
(defmethod reset ((iterator tree-iterator))
  (with-slots (depth subtrees positions) iterator
    (setf depth 0
          (pos iterator) 0
          (fill-pointer subtrees) 1
          (fill-pointer positions) 1)))

;; TODO test
(defmethod copy-iterator ((iterator tree-iterator)
                          &optional target)
  (flet ((copy-vec (vec)
           ;; TODO there's something in alexandria for this
           (make-array (length vec)
                       :element-type (array-element-type vec)
                       :adjustable t
                       :fill-pointer (fill-pointer vec)
                       :initial-contents vec)))
    (with-slots (subtrees positions depth) iterator
      (cond
        (target
         (setf (slot-value target 'subtrees) (copy-vec subtrees)
               (slot-value target 'positions) (copy-vec positions)
               (slot-value target 'depth) depth)
         target)
        (t
         (make-instance
          (class-of iterator)
          :subtrees (copy-vec subtrees)
          :positions (copy-vec positions)
          :depth depth))))))

(defmethod push-subtree ((iterator tree-iterator)
                        subtree &key (position 0))
  (with-slots (positions depth subtrees) iterator
    (vector-push-extend subtree subtrees)
    (vector-push-extend position positions)
    (setf depth (1- (length positions))))
  iterator)

(defmethod pop-subtree ((iterator tree-iterator))
  (with-slots (positions depth subtrees) iterator
    (decf (fill-pointer subtrees))
    (decf (fill-pointer positions))
    (decf depth))
  iterator)

(defmethod go-down ((iterator tree-iterator))
  (unless (current-depth-done-p iterator)
    (let ((value-to-dig-in (children iterator)))
      (when value-to-dig-in
        (push-subtree iterator
                      (if (eq t value-to-dig-in)
                          (value iterator)
                          value-to-dig-in))
        t))))

(defmethod go-up ((iterator tree-iterator))
  (unless (zerop (slot-value iterator 'depth))
    (pop-subtree iterator)
    t))

;; TODO add tests
(defmethod subtree-at-depth ((iterator tree-iterator) depth)
  (with-slots (subtrees) iterator
    (aref subtrees depth)))

(defmethod root-subtree ((iterator tree-iterator))
  (subtree-at-depth iterator 0))

;; TODO add tests
(defmethod goto-root ((iterator tree-iterator))
  (with-slots (positions depth subtrees) iterator
    (setf (fill-pointer subtrees) 1)
    (setf (fill-pointer positions) 1)
    (setf depth 0))
  iterator)

;; TODO add tests
(defmethod value-at-depth ((iterator tree-iterator) depth)
  (with-slots (positions subtrees) iterator
    (let ((pos (aref positions depth))
          (subtree (aref subtrees depth)))
      (aref subtree pos))))

;; TODO add tests
(defmethod parent-value ((iterator tree-iterator))
  (with-slots (depth) iterator
    (when (plusp depth)
      (value-at-depth iterator (1- depth)))))

;; TODO add tests
(defmethod root-value ((iterator tree-iterator))
  (value-at-depth iterator 0))


;;; Concat iterator
;;
;; TODO maybe find a better name
;; TODO add documentation
;; TODO add tests

(defclass concat-iterator (vector-iterator) ())

(defun make-concat-iterator (iterators)
  (make-instance 'concat-iterator
                 :vector (coerce iterators 'vector)))

(defmethod child-iterator ((iterator concat-iterator))
  (aref (slot-value iterator 'vector)
        (slot-value iterator 'positions)))

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
