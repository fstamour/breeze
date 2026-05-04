(in-package #:breeze.iterator)

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


;;; Other methods on tree-iterator

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
