(cl:in-package #:common-lisp-user)

#+ (or)
(ql:quickload '(#:value-semantics-utils))
;; ^^^ Failed, can't find `with-macroexpand-time-branching`

(defpackage #:functional-classes
  (:use #:cl))

(in-package #:functional-classes)

(defun be-paranoid (a b)
  "Signals an error when A and B are not equalp."
  (unless (equalp a b)
    (error "What!? these should be the equivalent!")))

;; So, let's say I have this tree that I want to modify
(defparameter *tree*
  (copy-tree ; because mutating a literal is not ideal
   `(defpackage #:functional-classes
      (:use #:cl))))

;; Let's say I want to add something, but keep the original intact.
;; The naive way is just to copy the whole thing and do the mutation.


(defparameter *new-tree*
  (let ((new-tree (copy-tree *tree*)))
    (be-paranoid new-tree *tree*)
    (setf (cdddr new-tree) `((:local-nicknames (:a :alexandria))))
    new-tree))
#+ (or)
(DEFPACKAGE #:FUNCTIONAL-CLASSES
  (:USE #:CL)
  (:LOCAL-NICKNAMES (:A :ALEXANDRIA)))

;; But this method (copying everything) fails to take advantage of
;; possible structural sharing.
;;
;; For example, both trees have equivalent third element.
(third *tree*)
;; => (:USE #:CL)
(equalp (nth 2 *tree*)
	(nth 2 *new-tree*))
;; => T

;; But they aren't actually the same
(eq (nth 2 *tree*)
    (nth 2 *new-tree*))
;; => NIL


;;; Concluion: it would be nice to have some utilities to copy just what
;;; needs to be copied.


;;; The problem is worst with nested objects as we need some method
;;; for deep copying the tree...
;;;
;;; See https://codereview.stackexchange.com/q/156392 for a possible
;;; implementation of a generic deep-copy.

(use-package :breeze.reader)
(use-package :breeze.refactor)

(parse-string
 "(defpackage #:functional-classes
      (:use #:cl))")
#|
(#<LIST-NODE (#<SYMBOL-NODE "(" "defpackage">
			    #<SYMBOL-NODE " " "#:functional-classes">
			    #<LIST-NODE "
      " (#<SYMBOL-NODE "(" ":use">
		       #<SYMBOL-NODE " " "#:cl">) :raw "(:use #:cl)">) :raw "(defpackage #:functional-classes
      (:use #:cl))">)
|#

;;; Can't even use copy-tree in this case...




(defclass nod ()
  ((data
    :initform nil
    :initarg :data
    :accessor nod-data)
   (children
    :initform nil
    :initarg :children
    :accessor nod-children)
   (dag
    :initform nil
    :initarg :dag
    :accessor nod-dag
    :documentation "The DAG in which this nod resides.")))

(defun make-nod (data &optional children)
  (make-instance 'nod
		 :data data
		 :children children))

(defun make-nods (&rest data &optional children)
  (make-instance 'nod
		 :data data
		 :children children))

(defmethod print-object ((nod nod) stream)
  (print-unreadable-object
      (nod stream :type t :identity nil)
    (format stream "~s" (nod-data nod))
    (when (nod-children nod)
      (format stream " ~s" (nod-children nod)))))

(make-nod 1)
;; #<NOD 1>

(make-nod 1 (list (make-nod 2)
		  (make-nod 3)))
;; #<NOD 1 (#<NOD 2> #<NOD 3>)>

(defclass dag ()
  ((roots
    :initform nil
    :initarg :roots
    :accessor dag-roots))
  (:documentation "A directed acyclic graph of nod."))

(defmethod print-object ((dag dag) stream)
  (print-unreadable-object
      (dag stream :type t :identity nil)
    (format stream "~{~s~^ ~}" (dag-roots dag))))

(defun make-dag ()
  (make-instance 'dag))

(make-dag)

(defun add-root (dag data)
  ;; TODO unlesss equivalent...
  (push (dag-roots (make-nod data))))

(defun dag-from-tree (tree)
  (if (listp tree)
      (dolist (node tree)
	(make-nod node))))

(dag-from-tree
 '(1))
