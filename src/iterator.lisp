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
           #:pre-order-iterator
           #:make-pre-order-iterator
           #:leaf-iterator
           #:make-leaf-iterator
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
   #:vector-at-depth
   #:root-vector
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
    :type vector
    :documentation "The vector being iterated over.")
   (position
    :initform 0
    :initarg :position
    :type integer
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
    (incf iterator)))

(defmethod value ((iterator vector-iterator))
  (with-slots (position vector) iterator
    (aref vector position)))

;; TODO test
(defmethod reset ((iterator vector-iterator))
  (with-slots (position) iterator
    (setf position 0)))

;; TODO test
(defmethod copy-iterator ((iterator vector-iterator))
  (with-slots (position vector) iterator
    (make-vector-iterator vector :position position)))


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


;;; iterator for nested vectors

(defclass nested-vector-iterator (iterator)
  ((vectors
    :initform (make-array '(0)
                          :element-type 'vector
                          :adjustable t
                          :fill-pointer t)
    :initarg :vectors)
   (depth
    :initform 0
    :initarg :depth
    :type fixnum)
   (positions
    :initform (make-array '(1)
                          :element-type 'fixnum
                          :adjustable t
                          :initial-element 0
                          :fill-pointer 0)
    :initarg :positions
    :type (vector fixnum)))
  (:documentation "An iterator for nested vectors."))

(defmethod print-object ((iterator nested-vector-iterator) stream)
  (print-unreadable-object
      (iterator stream :type nil :identity t)
    (with-slots (depth positions) iterator
        (format stream "nested-vector-iterator depth: ~s pos: ~s" depth positions))))

(defun make-nested-vector-iterator (vector)
  "Create a new depth-first iterator on VECTOR."
  (check-type vector vector)
  (push-vector (make-instance 'nested-vector-iterator) vector))

(declaim (inline vec))
(defun vec (iterator)
  #++ (declare (optimize (speed 3)))
  #|
  unable to
    optimize
  because:
    Upgraded element type of array is not known at compile time.
  |#
  (with-slots (vectors depth) iterator
      (aref vectors depth)))

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

(declaim (inline current-depth-done-p))
(defun current-depth-done-p (iterator)
  (not (< -1 (pos iterator) (length (vec iterator)))))

;; same implementation as vector-iterator's
(defmethod donep ((iterator nested-vector-iterator))
  (with-slots (depth) iterator
    (and (zerop depth)
         (current-depth-done-p iterator))))

;; same implementation as vector-iterator's
(defmethod next ((iterator nested-vector-iterator) &key)
  (incf (pos iterator)))

;; same implementation as vector-iterator's
(defmethod value ((iterator nested-vector-iterator))
  (aref (vec iterator) (pos iterator)))

;; TODO test
(defmethod reset ((iterator nested-vector-iterator))
  (with-slots (depth vectors positions) iterator
    (setf depth 0
          (pos iterator) 0
          (fill-pointer vectors) 1
          (fill-pointer positions) 1)))

;; TODO test
(defmethod copy-iterator ((iterator nested-vector-iterator))
  (flet ((copy-vec (vec)
           ;; TODO there's something in alexandria for this
            (make-array (length vec)
                     :element-type (array-element-type vec)
                     :adjustable t
                     :fill-pointer (fill-pointer vec)
                     :initial-contents vec)))
    (with-slots (vectors positions depth) iterator
      (make-instance
       'nested-vector-iterator
       :vectors (copy-vec vectors)
       :positions (copy-vec positions)
       :depth depth))))

(defmethod push-vector ((iterator nested-vector-iterator)
                        vector &key (position 0))
  (with-slots (positions depth vectors) iterator
    (vector-push-extend vector vectors)
    (vector-push-extend position positions)
    (setf depth (length positions)))
  iterator)

(defmethod pop-vector ((iterator nested-vector-iterator))
  (with-slots (positions depth vectors) iterator
    (decf (fill-pointer vectors))
    (decf (fill-pointer positions))
    (decf depth))
  iterator)

;; TODO add tests
(defmethod vector-at-depth ((iterator nested-vector-iterator) depth)
  (with-slots (vectors) iterator
    (aref vectors depth)))

(defmethod root-vector ((iterator nested-vector-iterator))
  (vector-at-depth iterator 0))

;; TODO add tests
(defmethod value-at-depth ((iterator nested-vector-iterator) depth)
  (with-slots (positions vectors) iterator
    (let ((pos (aref positions depth))
          (vec (aref vectors depth)))
      (aref vec pos))))

;; TODO add tests
(defmethod parent-value ((iterator nested-vector-iterator))
  (with-slots (depth) iterator
    (when (plusp depth)
      (value-at-depth iterator (1- depth)))))

;; TODO add tests
(defmethod root-value ((iterator nested-vector-iterator))
  (value-at-depth iterator 0))


;;; Depth-first iterator

(defclass recursive-iterator (nested-vector-iterator)
  ((recurse-into
    :initform nil
    :initarg :recurse-into
    :accessor recurse-into
    :documentation "Callback to control which value to recurse into."))
  (:documentation "Iterator for nested vectors that automatically recurse into subtrees."))

(defun make-recursive-iterator (vector recurse-into
                                &rest rest
                                &key
                                  apply-recurse-into-to-iterator-p
                                  (class 'recursive-iterator)
                                &allow-other-keys)
  (let ((iterator
          (apply
           #'make-instance
           class
           :recurse-into (if apply-recurse-into-to-iterator-p
                             recurse-into
                             (lambda (iterator)
                               ;; TODO I think this =unless= is
                               ;; unecesary if I fix the function
                               ;; "maybe-dig-in"
                               (unless (current-depth-done-p iterator)
                                 (funcall recurse-into (value iterator)))))
           (alexandria:remove-from-plist rest :class :apply-recurse-into-to-iterator-p))))
    (push-vector iterator vector)
    iterator))



(defclass pre-order-iterator (recursive-iterator) ())

(defun make-pre-order-iterator (vector recurse-into
                                &key apply-recurse-into-to-iterator-p)
  (make-recursive-iterator
   vector recurse-into
   :apply-recurse-into-to-iterator-p apply-recurse-into-to-iterator-p
   :class 'pre-order-iterator))


(defmethod next ((iterator pre-order-iterator) &key dont-recurse-p)
  (with-slots (positions depth) iterator
    (breeze.logging:log-debug "next(pre-order): ~s" positions)
    (labels ((maybe-dig-in (iterator)
               (unless (current-depth-done-p iterator)
                 (let ((digged-in-p nil))
                   (loop :for value-to-dig-in = (funcall (recurse-into iterator) iterator)
                         :for value = (and value-to-dig-in
                                           (if (eq t value-to-dig-in)
                                               (value iterator)
                                               value-to-dig-in))
                         :while (and value
                                     ;; don't "dig-in" if the value-to-dig-in is empty
                                     (plusp (length value)))
                         :do
                            (setf digged-in-p t)
                            (push-vector iterator value))
                   digged-in-p)))
             (maybe-dig-out (iterator)
               ;; TODO this docstring is not exactly right, it's still written as
               ;; if iterator was like a linked list (it used to be).
               "If ITERATOR is done and has a parent, return the next
parent (i.e. the parent might also be done, in which case the parent's
parents is returned, and so on and so on). Otherwise, just return
ITERATOR unchanged."
               (loop
                 ;; :with digged-out-p = nil
                 :while (and (current-depth-done-p iterator) (plusp depth))
                 :do (pop-vector iterator) (next iterator :dont-recurse-p t)
                     ;; :finally (when digged-out-p (next iterator))
                 )))
      (flet ((++ ()
               (breeze.logging:log-debug "next: BEFORE ++ ~s" positions)
               (incf (pos iterator))
               (breeze.logging:log-debug "next: AFTER ++ ~s" positions))
             (out ()
               (progn
                 (breeze.logging:log-debug "next: BEFORE dig out ~s" positions)
                 (maybe-dig-out iterator)
                 (breeze.logging:log-debug "next: AFTER dig out ~s" positions)))
             (in ()
               (progn
                 (breeze.logging:log-debug "next: BEFORE dig in ~s" positions)
                 (let ((digged-in-p (maybe-dig-in iterator)))
                   (breeze.logging:log-debug "next: AFTER dig in ~s (digged-in-p: ~s)" positions
                                             digged-in-p)
                   digged-in-p))))
        ;; if the current node is "recursable", then we recurse into it
        (cond
          (dont-recurse-p (++) (out))
          ((not dont-recurse-p)
           (cond
             ((in)
              ;; If we "digged-in", we don't want to increment the current
              ;; position, or it'll skip the first child of the sequence
              ;; we're recursed into.
              nil)
             (t (++)))
           ;; "dig out" whether we digged in or not.
           (out)))))))


(defmethod next ((iterator pre-order-iterator) &key dont-recurse-p)
  (with-slots (depth) iterator
      (labels ((next-sibling ()
                 (incf (pos iterator)) (maybe-dig-out)
                 (values))
               (maybe-dig-in ()
                 (unless (current-depth-done-p iterator)
                   (let ((digged-in-p nil))
                     (loop :for value-to-dig-in = (funcall (recurse-into iterator) iterator)
                           :for value = (and value-to-dig-in
                                             (if (eq t value-to-dig-in)
                                                 (value iterator)
                                                 value-to-dig-in))
                           :while (and value
                                       ;; don't "dig-in" if the value-to-dig-in is empty
                                       (plusp (length value)))
                           :do
                              (setf digged-in-p t)
                              (push-vector iterator value))
                     digged-in-p)))
               (maybe-dig-out ()
                 (loop
                   :while (and (current-depth-done-p iterator) (plusp depth))
                   :do (pop-vector iterator) (next-sibling))
                 (values)))
        ;; if the current node is "recursable", then we recurse into it
        (if dont-recurse-p
            (next-sibling)
            (unless (maybe-dig-in)
              (incf (pos iterator))))
        (maybe-dig-out)
        #++
        (cond
          (dont-recurse-p (next-sibling))
          ((not dont-recurse-p)
           (cond
             ((maybe-dig-in)
              ;; If we "digged-in", we don't want to increment the current
              ;; position, or it'll skip the first child of the sequence
              ;; we're recursed into.
              nil)
             (t (incf (pos iterator))))
           ;; "dig out" whether we digged in or not.
           (maybe-dig-out))))))



(declaim (inline maybe-dig-in))
(defun maybe-dig-in (iterator)
  (unless (current-depth-done-p iterator)
    (let ((digged-in-p nil))
      (loop :for value-to-dig-in = (funcall (recurse-into iterator) iterator)
            :for value = (and value-to-dig-in
                              (if (eq t value-to-dig-in)
                                  (value iterator)
                                  value-to-dig-in))
            :while value
            :do (setf digged-in-p t)
                (push-vector iterator value))
      digged-in-p)))

(declaim (inline maybe-dig-out))
(defun maybe-dig-out (iterator)
  ;; TODO this docstring is not exactly right, it's still written as
  ;; if iterator was like a linked list (it used to be).
  "If ITERATOR is done and has a parent, return the next
parent (i.e. the parent might also be done, in which case the parent's
parents is returned, and so on and so on). Otherwise, just return
ITERATOR unchanged."
  (with-slots (depth) iterator
    (loop
      ;; :with digged-out-p = nil
      :while (and (current-depth-done-p iterator) (plusp depth))
      :do (pop-vector iterator) (next iterator :dont-recurse-p t)
          ;; :finally (when digged-out-p (next iterator))
      )))

(defclass leaf-iterator (recursive-iterator) ())

(defun make-leaf-iterator (vector recurse-into
                                   &key apply-recurse-into-to-iterator-p)
  (let ((iterator (make-recursive-iterator
                   vector recurse-into
                   :apply-recurse-into-to-iterator-p apply-recurse-into-to-iterator-p
                   :class 'leaf-iterator)))
    (next-non-empty-subtree iterator)
    iterator))

(defun next-non-empty-subtree (iterator)
  (with-slots (positions) iterator
    (breeze.logging:log-debug "==next-non-empty-subtree==" positions)
    (flet ((out ()
             (progn
               (breeze.logging:log-debug "next-non-empty-subtree: BEFORE dig out ~s" positions)
               (maybe-dig-out iterator)
               (breeze.logging:log-debug "next-non-empty-subtree: AFTER dig out ~s" positions)))
           (in ()
             (progn
               (breeze.logging:log-debug "next-non-empty-subtree: BEFORE in ~s" positions)
               (maybe-dig-in iterator)
               (breeze.logging:log-debug "next-non-empty-subtree: AFTER in ~s" positions))))
      ;; the loop is necessary to skip over "deeply empty" trees. For
      ;; example: #(#(#(#())))
      (loop :while (in) :do (out)))))

(defmethod next ((iterator leaf-iterator) &key dont-recurse-p)
  (with-slots (positions) iterator
    (breeze.logging:log-debug "next(leaf): ~s" positions)
    (flet ((++ ()
             (breeze.logging:log-debug "next: BEFORE ++ ~s" positions)
             (incf (pos iterator))
             (breeze.logging:log-debug "next: AFTER ++ ~s" positions))
           (out ()
             (progn
               (breeze.logging:log-debug "next: BEFORE dig out ~s" positions)
               (maybe-dig-out iterator)
               (breeze.logging:log-debug "next: AFTER dig out ~s" positions)))
           (in ()
             (progn
               (breeze.logging:log-debug "next: BEFORE next-non-empty-subtree ~s" positions)
               (next-non-empty-subtree iterator)
               (breeze.logging:log-debug "next: AFTER next-non-empty-subtree ~s" positions))))
      (++)
      (out)
      (unless dont-recurse-p (in)))))


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
