(in-package #:breeze.iterator)

(defparameter *initial-tree-iterator-array-length* 8
  "How many levels a tree-iterator can initially hold before its
`subtrees' and `positions' have to grow (see `push-subtree').")

(deftype subtrees-vector ()
  "The type of a tree-iterator's `subtrees'."
  'simple-vector)

(deftype positions-vector ()
  "The type of a tree-iterator's `positions'."
  '(simple-array fixnum (*)))

(defun make-subtrees (&optional (length *initial-tree-iterator-array-length*))
  (make-array (list length) :initial-element #()))

(defun make-positions (&optional (length *initial-tree-iterator-array-length*))
  (make-array (list length)
              :element-type 'fixnum
              :initial-element -1))

(defclass tree-iterator (iterator)
  ((subtrees
    :initform (make-subtrees)
    :initarg :subtrees
    :type subtrees-vector
    :documentation "A vector containing the current subtree being at each level

For example, the first element is the root of the tree, and the last
active element is current subtree).

Only the first (1+ `depth') elements are \"active\" (the slot `depth' is
used as a fill-pointer).")
   (positions
    :initform (make-positions)
    :initarg :positions
    :type positions-vector
    :documentation "A vector of positions, each one representing the position at different
depth of the tree.

Only the first (1+ `depth') elements are \"active\" (the slot `depth' is
used as a fill-pointer).")
   (depth
    :initform -1
    :initarg :depth
    :type fixnum
    :documentation "The index, in `subtrees' and `positions', of the current location the
iterator is currently at. -1 means that no subtree was pushed."))
  (:documentation "An iterator for nested subtrees."))

(declaim (inline depth))
(declaim (ftype (function (tree-iterator) fixnum) depth))
(defun depth (iterator)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (the fixnum (slot-value iterator 'depth)))

;; TODO I should document why this isn't an :after method
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
  (with-slots ((pos-a positions) (subtrees-a subtrees) depth) a
    (with-slots ((pos-b positions) (subtrees-b subtrees)) b
      (when (= depth (depth b))
        (loop :for d :to depth
              :always (and (eq (svref subtrees-a d)
                               (svref subtrees-b d))
                           (= (aref pos-a d)
                              (aref pos-b d))))))))

(defmethod children ((iterator tree-iterator))
  (let ((value (value iterator)))
    (when (vectorp value) value)))

(declaim (inline subtree))
(declaim (ftype (function (tree-iterator) t) subtree))
(defun subtree (iterator)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type tree-iterator iterator))
  (with-slots (subtrees depth) iterator
    (declare (type subtrees-vector subtrees)
             (type fixnum depth))
    (svref subtrees depth)))

(declaim (inline pos))
(declaim (ftype (function (tree-iterator) fixnum) pos))
(defun pos (iterator)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type tree-iterator iterator))
  (with-slots (positions depth) iterator
    (declare (type positions-vector positions)
             (type fixnum depth))
    (aref positions depth)))

(declaim (inline (setf pos)))
(defun (setf pos) (new-position iterator)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type fixnum new-position)
           (type tree-iterator iterator))
  (with-slots (positions depth) iterator
    (declare (type positions-vector positions)
             (type fixnum depth))
    (setf (aref positions depth) new-position)))

;; TODO add tests
(declaim (inline position-at-depth))
(defun position-at-depth (iterator depth)
  "Get the position of ITERATOR at depth DEPTH."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type tree-iterator iterator)
           (type fixnum depth))
  (with-slots (positions) iterator
    (declare (type positions-vector positions))
    (aref positions depth)))

(defun active-positions (iterator)
  "Get a fresh vector of the positions that ITERATOR is actually using,
one per level, from the root to the current depth."
  (subseq (slot-value iterator 'positions) 0 (1+ (depth iterator))))

(defmethod current-position ((iterator tree-iterator))
  (pos iterator))

(defmethod (setf current-position) (new-position (iterator tree-iterator))
  (setf (pos iterator) new-position))

(declaim (inline current-depth-done-p))
(defun current-depth-done-p (iterator)
  (declare (type tree-iterator iterator))
  (or
   ;; no subtree was pushed (or they were all popped)
   (minusp (depth iterator))
   (let ((subtree (subtree iterator))
         (pos (pos iterator)))
     (declare (type fixnum pos))
     (if (vectorp subtree)
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

(defmethod print-object ((iterator tree-iterator) stream)
  (print-unreadable-object
      (iterator stream :type t :identity t)
    (format stream "depth=~s pos=~s"
            (depth iterator) (active-positions iterator))
    (if (tree-iterator-done-p iterator)
        (write-string " :done" stream)
        (let ((*print-level* 2)
              (*print-length* 4)
              (*print-pretty* nil)
              (*print-right-margin* nil))
          (write-string " value=" stream)
          (write-string
           (breeze.string:around
            (prin1-to-string (tree-iterator-value iterator))
            0 40)
           stream)))))

;; TODO test
(defmethod reset ((iterator tree-iterator))
  "Reset a tree-iterator.

This is like `goto-root' followed by `goto-first' (`goto-first'
doesn't exist)."
  (with-slots (depth) iterator
    (setf depth 0
          (pos iterator) 0)))

;; TODO test
(defmethod copy-iterator ((iterator tree-iterator)
                          &optional target)
  (with-slots (subtrees positions depth) iterator
    (cond
      (target
       (with-slots ((target-subtrees subtrees)
                    (target-positions positions)
                    (target-depth depth))
           target
         (let ((length (length subtrees)))
           (cond
             ;; copy the vectors for the target when they are too small
             ((< (length target-subtrees) length)
              (setf target-subtrees (copy-seq subtrees)
                    target-positions (copy-seq positions)))
             (t
              (replace target-subtrees subtrees)
              (replace target-positions positions))))
         (setf target-depth depth))
       target)
      (t
       (make-instance
        (class-of iterator)
        :subtrees (copy-seq subtrees)
        :positions (copy-seq positions)
        :depth depth)))))

(defmethod push-subtree ((iterator tree-iterator)
                         subtree &key (position 0))
  (with-slots (positions subtrees depth) iterator
    (incf depth)
    (when (<= (length subtrees) depth)
      (let ((length (max (* 2 (length subtrees))
                         *initial-tree-iterator-array-length*)))
        (setf subtrees (replace (make-subtrees length) subtrees)
              positions (replace (make-positions length) positions))))
    (setf (svref subtrees depth) subtree
          (aref positions depth) position))
  iterator)

(defmethod pop-subtree ((iterator tree-iterator))
  (with-slots (depth) iterator
    (decf depth))
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
  "If ITERATOR's current value has children, move the iterator \"down\"
into the sequence of children.
Return `t' if if the iterator was moved.
Return `nil' otherwise (could not go down)."
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
  (when (plusp (depth iterator))
    (pop-subtree iterator)
    t))

;; TODO add tests
(defmethod goto-root ((iterator tree-iterator))
  "Move the ITERATOR to the root."
  (with-slots (depth) iterator
    (setf depth 0))
  iterator)

(defun next-up (iterator)
  (declare (optimize (speed 3)))
  (loop
    :for i :of-type fixnum :from 0
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
    :while (and (not (donep iterator))
                (= initial-pos
                   (position-at-depth iterator initial-depth)))
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
    (svref subtrees depth)))

(defmethod root-subtree ((iterator tree-iterator))
  "Get the root tree of ITERATOR."
  (subtree-at-depth iterator 0))

;; TODO add tests
(defmethod value-at-depth ((iterator tree-iterator) depth)
  (declare (type fixnum depth))
  (let ((pos (position-at-depth iterator depth))
        (subtree (subtree-at-depth iterator depth)))
    (aref subtree pos)))

;; TODO add tests
(defmethod iterator-at-depth ((iterator tree-iterator) depth)
  ;; TODO _maybe_ some checks
  (let ((new (copy-iterator iterator)))
    (with-slots ((current-depth depth)) new
      (setf current-depth depth))
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
