#|

Iterator on vectors and nested vectors (trees).

Because both the syntax trees and the (compiled) patterns are nested
vectors.

Historical note: this started as a struct in pattern.lisp. This is a
generalization of that first iteration (ha!).

|#


;;; breeze.iterator package definition

(uiop:define-package #:breeze.iterator
  (:documentation "Iterators for nested vectors")
  (:use #:cl)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:breeze.class-utils
                #:define-class)
  ;; Generics
  (:export #:donep
           #:next
           #:value
           #:reset
           #:copy-iterator
           #:current-position)
  ;; Classes and Constructors
  (:export #:iterator
           #:vector-iterator
           #:tree-iterator
           #:make-vector-iterator
           #:make-tree-iterator
           #:concat-iterator
           #:make-concat-iterator
           #:stream-iterator
           #:make-stream-iterator)
  ;; Accessors
  (:export
   #:tree-iterator-done-p
   #:tree-iterator-value
   #:tree-iterator-next
   #:pos
   #:subtree
   #:subtrees
   #:positions
   #:children
   #:push-subtree
   #:pop-subtree
   #:go-forward
   #:go-backward
   #:go-down
   #:go-up
   #:next-up
   #:next-preorder
   #:subtree-at-depth
   #:root-subtree
   #:goto-root
   #:next-up
   #:value-at-depth
   #:parent-value
   #:parent
   #:root-value
   #:rootp
   #:root
   #:previous-sibling
   #:next-sibling
   #:previous-iterator
   #:next-iterator
   #:map-subtree-preorder)
  ;; Other utility functions
  (:export
   #:firstp
   #:lastp
   #:before-last-p
   #:depth
   #:collect
   #:map-iterator
   #:iterator-value))

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


;;; iterator for trees (nested vectors)

(defparameter *initial-tree-iterator-array-size* 0)

(defun make-subtrees ()
  (make-array
   (list *initial-tree-iterator-array-size*)
   :element-type 'vector
   :initial-element #()
   :adjustable t
   :fill-pointer 0))

(defun make-positions ()
  (make-array
   (list *initial-tree-iterator-array-size*)
   :element-type 'fixnum
   :initial-element -1
   :adjustable t
   :fill-pointer 0))

(defclass tree-iterator (iterator)
  ((subtrees
    :initform (make-subtrees)
    :initarg :subtrees
    :type vector
    :documentation "A vector containing the current subtree being at each level

For example, the first element is the root of the tree, and the last
active element is current subtree).")
   (positions
    :initform (make-positions)
    :initarg :positions
    :type (vector fixnum)
    :documentation "A vector of positions, each one representing the position at different
depth of the tree."))
  (:documentation "An iterator for nested subtrees."))

(declaim (inline depth))
(declaim (ftype (function (tree-iterator) fixnum) depth))
(defun depth (iterator)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (with-slots (positions) iterator
    (declare (type (vector fixnum) positions))
    (1- (length positions))))

(defmethod print-object ((iterator tree-iterator) stream)
  (print-unreadable-object
      (iterator stream :type t :identity t)
    (with-slots (positions) iterator
      (format stream "depth: ~s pos: ~s" (depth iterator) positions))))

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

(defmethod eqv ((a tree-iterator) (b tree-iterator))
  (with-slots ((pos-a positions) (subtrees-a subtrees)) a
    (with-slots ((pos-b positions) (subtrees-b subtrees)) b
      (and (= (depth a) (depth b))
           (every #'eq subtrees-a subtrees-b)
           (every #'= pos-a pos-b)))))

(defmethod children ((iterator tree-iterator))
  (let ((value (value iterator)))
    (when (vectorp value) value)))

(declaim (inline subtree))
(declaim (ftype (function (tree-iterator) t) subtree))
(defun subtree (iterator)
  (declare (optimize (speed 1) (safety 1) (debug 0)))
  (declare (type tree-iterator iterator))
  #|
  unable to  optimize because:
  Upgraded element type of array is not known at compile time.
  |#
  (with-slots (subtrees) iterator
    (declare (type vector subtrees))
    (aref subtrees (depth iterator))))

(declaim (inline pos))
(declaim (ftype (function (tree-iterator) fixnum) pos))
(defun pos (iterator)
  (declare (optimize (speed 1) (safety 1) (debug 0)))
  ;; (declare (optimize (speed 3) (safety 1) (debug 0)))
  #|
    unable to optimize
    due to type uncertainty: The first argument is a (VECTOR FIXNUM), not a SIMPLE-ARRAY.
  |#
  (with-slots (positions) iterator
    (declare (type (vector fixnum) positions))
    (aref positions (depth iterator))))

(declaim (inline (setf pos)))
(defun (setf pos) (new-position iterator)
  (with-slots (positions) iterator
    (setf (aref positions (depth iterator)) new-position)))

(defmethod current-position ((iterator tree-iterator))
  (pos iterator))

(defmethod (setf current-position) (new-position (iterator tree-iterator))
  (setf (pos iterator) new-position))

(declaim (inline current-depth-done-p))
(defun current-depth-done-p (iterator)
  (declare (type tree-iterator iterator))
  (or
   ;; too deep
   (with-slots (subtrees) iterator
     (declare (type vector subtrees))
     (<= (length subtrees) (depth iterator)))
   (let ((subtree (subtree iterator))
         (pos (pos iterator)))
     (declare (type fixnum pos)
              (dynamic-extent pos subtree))
     (if (typep subtree 'sequence)
         (locally
             (declare (type vector subtree))
           (not (< -1 pos (length subtree))))
         ;; if the subtree is not a vector, then 0 is the only valid
         ;; position
         (plusp pos)))))

(declaim (inline tree-iterator-done-p))
(defun tree-iterator-done-p (iterator)
  (declare (type tree-iterator iterator))
  (current-depth-done-p iterator))

(defmethod donep ((iterator tree-iterator))
  (tree-iterator-done-p iterator))

(declaim (inline tree-iterator-next))
(defun tree-iterator-next (iterator)
  (declare (type tree-iterator iterator))
  (incf (pos iterator)))

(defmethod next ((iterator tree-iterator))
  (tree-iterator-next iterator))

(declaim (inline tree-iterator-value))
(defun tree-iterator-value (iterator)
  (declare (type tree-iterator iterator)
           ;; (optimize (speed 3))
           )
  (let ((subtree (subtree iterator)))
    (if (vectorp subtree)
        (aref subtree (pos iterator))
        ;; If we ever need more flexibility: replace ~subtree~ by
        ;; ~(value subtree)~ and let the "client" define other `value'
        ;; methods.
        subtree)))

(defmethod value ((iterator tree-iterator))
  (tree-iterator-value iterator))

;; TODO test
(defmethod reset ((iterator tree-iterator))
  "Reset a tree-iterator.

This is like `goto-root' followed by `goto-first' (`goto-first'
doesn't exist)."
  (with-slots (subtrees positions) iterator
    (setf (fill-pointer subtrees) 1
          (fill-pointer positions) 1
          (pos iterator) 0)))

;; TODO test
(defmethod copy-iterator ((iterator tree-iterator)
                          &optional target)
  (flet ((copy-vec (vec)
           (make-array (length vec)
                       :element-type (array-element-type vec)
                       :adjustable t
                       :fill-pointer (fill-pointer vec)
                       :initial-contents vec)))
    (with-slots (subtrees positions) iterator
      (cond
        (target
         (setf (slot-value target 'subtrees) (copy-vec subtrees)
               (slot-value target 'positions) (copy-vec positions))
         target)
        (t
         (make-instance
          (class-of iterator)
          :subtrees (copy-vec subtrees)
          :positions (copy-vec positions)))))))

(defmethod push-subtree ((iterator tree-iterator)
                        subtree &key (position 0))
  (with-slots (positions subtrees) iterator
    (vector-push-extend subtree subtrees)
    (vector-push-extend position positions))
  iterator)

(defmethod pop-subtree ((iterator tree-iterator))
  (with-slots (positions subtrees) iterator
    (decf (fill-pointer subtrees))
    (decf (fill-pointer positions)))
  iterator)

;; TODO test
(defmethod go-forward ((iterator tree-iterator) (n integer))
  (let ((pos (pos iterator))
        (length (length (subtree iterator))))
    (when (< pos length)
      (incf (pos iterator) (min n (- length pos)))
      t)))

;; TODO test
;; TODO what happens if n is 0 or negative?
(defmethod go-backward ((iterator tree-iterator) (n integer))
  (let ((pos (pos iterator)))
    (when (plusp pos)
      (decf (pos iterator) (min n pos))
      t)))

(defmethod go-down ((iterator tree-iterator))
  ;; (declare (optimize (speed 3) (safety 1) (debug 0)))
  (unless (current-depth-done-p iterator)
    (let ((value-to-dig-in (children iterator)))
      (when value-to-dig-in
        (push-subtree iterator
                      (if (eq t value-to-dig-in)
                          (value iterator)
                          value-to-dig-in))
        t))))

(defmethod go-up ((iterator tree-iterator))
  (unless (zerop (depth iterator))
    (pop-subtree iterator)
    t))

;; TODO add tests
(defmethod goto-root ((iterator tree-iterator))
  "Move the ITERATOR to the root."
  (with-slots (positions subtrees) iterator
    (setf (fill-pointer subtrees) 1)
    (setf (fill-pointer positions) 1))
  iterator)

(defun next-up (iterator)
  (declare (optimize (speed 3)))
  (loop
    :for i :from 0
    :while (and (current-depth-done-p iterator)
                (plusp (depth iterator)))
    :do
       ;; go-up
       (pop-subtree iterator)
       ;; go-forward
       (next iterator)
    :finally (return (when (plusp i) i))))

(defun next-preorder (iterator)
  (unless (go-down iterator) (next iterator))
  (next-up iterator))

;; WIP this is used by the "proof-of-concept on how to take account of
;; the indentation when inserting something." in src/refactor.lisp
(defun next-preorder* (iterator hook-down hook-up)
  (if (go-down iterator)
      (funcall hook-down)
      (next iterator))
  (loop
    :with went-down
    :while (and (current-depth-done-p iterator)
                (plusp (depth iterator)))
    :do
       ;; go-up
       (pop-subtree iterator)
       ;; go-forward
       (next iterator)
       (setf went-down t)
    :finally (when went-down
               (funcall hook-up))))

(defmethod map-subtree-preorder ((iterator tree-iterator)
                                 callback)
  "Map over the subtree designated by ITERATOR, applying CALLBACK to the
parent node then to the children."
  (loop
    :with initial-depth = (depth iterator)
    :with initial-pos = (pos iterator)
    :with positions = (slot-value iterator 'positions)
    :while (and (not (donep iterator))
                (= initial-pos
                   (aref positions initial-depth)))
    :do (funcall callback iterator)
        (next-preorder iterator)))


;;; Other methods on vector-iterator

;; TODO tests
(defmethod firstp ((iterator tree-iterator))
  "Is the current value the first one at the current depth?"
  (zerop (pos iterator)))

;; TODO tests
(defmethod lastp ((iterator tree-iterator))
  "Is the current value the last one at the current depth?"
  (let ((pos (pos iterator))
        (subtree (subtree iterator)))
    (etypecase subtree
      (vector (= pos (1- (length subtree))))
      (t t))))

(defmethod rootp ((iterator tree-iterator))
  "Test if ITERATOR is currently at the root of the tree."
  (zerop (depth iterator)))

;; TODO add tests
(defmethod subtree-at-depth ((iterator tree-iterator) depth)
  "Get the subtree of ITERATOR at depth DEPTH."
  (declare (type fixnum depth))
  (with-slots (subtrees) iterator
    (aref subtrees depth)))

(defmethod root-subtree ((iterator tree-iterator))
  "Get the root tree of ITERATOR."
  (subtree-at-depth iterator 0))

;; TODO add tests
(defmethod value-at-depth ((iterator tree-iterator) depth)
  (declare (type fixnum depth))
  (with-slots (positions subtrees) iterator
    (let ((pos (aref positions depth))
          (subtree (aref subtrees depth)))
      (aref subtree pos))))

;; TODO add tests
(defmethod iterator-at-depth ((iterator tree-iterator) depth)
  ;; TODO _maybe_ some checks
  (let ((new (copy-iterator iterator)))
    (with-slots (subtrees positions) new
      (setf
       (fill-pointer subtrees) 1
       (fill-pointer positions) 1))
    new))

;; TODO add tests
(defmethod parent-value ((iterator tree-iterator))
  (let ((depth (depth iterator)))
    (when (plusp depth)
      (value-at-depth iterator (1- depth)))))

;; TODO add tests
(defmethod parent ((iterator tree-iterator))
  (let ((depth (depth iterator)))
    (when (plusp depth)
      (iterator-at-depth iterator (1- depth)))))

;; TODO add tests
(defmethod root-value ((iterator tree-iterator))
  (value-at-depth iterator 0))

;; TODO add tests
(defmethod root ((iterator tree-iterator))
  "Get a copy of ITERATOR but at the root."
  (iterator-at-depth iterator 0))

;; TODO tests
(defmethod previous-sibling ((iterator tree-iterator))
  "Get the previous value at the same depth, or nil if there's is none."
  (unless (firstp iterator)
    (let ((pos (pos iterator))
          (subtree (subtree iterator)))
      (etypecase subtree
        (vector (aref subtree (1- pos)))
        (t nil)))))

(defmethod previous-sibling ((iterator (eql nil)))
  "Get the previous value at the same depth, or nil if there's is none."
  nil)

;; TODO tests
(defmethod next-sibling ((iterator tree-iterator))
  "Get the next value at the same depth, or nil if there's is none."
  (unless (lastp iterator)
    (let ((pos (pos iterator))
          (subtree (subtree iterator)))
      (etypecase subtree
        (vector (aref subtree (1+ pos)))
        (t nil)))))

(defmethod next-iterator ((iterator iterator))
  "Get a copy of ITERATOR moved to the next position."
  (let ((it (copy-iterator iterator)))
    (next it)
    (unless (donep it) it)))

(defmethod previous-iterator ((iterator iterator))
  "Get a copy of ITERATOR and move it backward to the previous
position (if it exists)."
  ;; TODO perf: this copies the iterator regarless if there is a
  ;; previous element or not.
  (let ((it (copy-iterator iterator)))
    (and (go-backward it 1) it)))


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

(defmethod next ((iterator concat-iterator))
  (loop :do (next (child-iterator iterator))
            (when (donep (child-iterator iterator))
              (incf (pos iterator)))
        :until (or (donep iterator)
                   (not (donep (child-iterator iterator))))))

(defmethod value ((iterator concat-iterator))
  (value (call-next-method iterator)))



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



(defclass stream-iterator (vector-iterator)
  ((stream
    :initform nil
    :initarg :stream
    :documentation "The source stream."))
  ;; Wordsoup!
  (:documentation "An adapter iterator for streams."))


(defun make-stream-iterator
    (stream &optional (buffer (make-array
                               '(0)
                               :element-type 'character
                               :adjustable t
                               :fill-pointer t)))
  (check-type stream stream)
  (assert (input-stream-p stream) (stream))
  (check-type buffer (vector character))
  (assert (adjustable-array-p buffer) (buffer))
  (make-instance 'stream-iterator
                 :stream stream
                 :vector buffer))

(defmethod donep ((iterator stream-iterator))
  (with-slots (position vector stream) iterator
    (if (< -1 position (length vector))
        nil
        (not (peek-char nil stream nil)))))

#+sbcl (declaim (sb-ext:maybe-inline read-next-char))
(defun read-next-char (stream-iterator)
  (with-slots (vector stream) stream-iterator
    (let ((char (read-char stream)))
      (vector-push-extend char vector)
      char)))

(defmethod value ((iterator stream-iterator))
  (declare (inline read-next-char))
  (with-slots (position vector stream) iterator
    (if (< -1 position (length vector))
        (aref vector position)
        (read-next-char iterator))))

#++
(with-input-from-string (in "Hello")
  (let ((it (make-stream-iterator in)))
    (collect it)
    #++ (list (donep it)
          (value it)
          (donep it)
          (value it))
    (slot-value it 'vector)))



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
