(uiop:define-package #:breeze.egraph
    (:documentation "Equivalence graphs to compactly represent lots of code.

- egraphs ≅ set of eclasses
- eclasses ≅ set of enodes
- enode ≅ an operator + operands eclasses (not enodes)")
  (:use #:cl)
  ;; Eclass
  (:export
   #:make-eclass
   #:id
   #:enodes
   #:parents)
  ;; Egraph
  (:export
   #:make-egraph
   #:enode-eclasses
   #:eclasses
   #:union-find
   #:eclasses
   #:pending)
  (:export
   #:eclass
   #:eclass-id
   #:eclass-find
   #:cannonicalize
   #:egraph-add-enode
   #:merge-eclass
   #:rebuild)
  (:export
   #:root-eclasses
   #:enode<
   #:smallest-enodes)
  (:export
   #:add-form))

(in-package #:breeze.egraph)


;;; Disjoint-sets data structure

(defun make-disjoint-sets (&optional number-of-sets)
  "Create a set of sets represented as an array.

Examples:
(make-disjoint-sets)
=> #()

(make-disjoint-sets 10)
;; => #(0 1 2 3 4 5 6 7 8 9)
"
  (let ((sets (make-array (list (or number-of-sets 0))
                          :element-type 'integer
                          :adjustable t
                          :fill-pointer t)))
    (when number-of-sets
      (loop :for i :below number-of-sets
            :do (setf (aref sets i) i)))
    sets))

(defun disjoint-sets-add (sets)
  "Add a new item into its own disjoint set. Return a new id.

Example:

(let ((id (disjoint-sets-add sets)))
  ;; SETS is modified
  ...)
"
  (let ((new-id (length sets)))
    (vector-push-extend new-id sets)
    new-id))

(defun disjoint-sets-find (sets id)
  "Find the id of the set representative (the root).

Example:

(disjoint-sets-find sets 5)
"
  (let ((parent (aref sets id)))
    (if (= id parent)
        ;; If "id" is the root, just return it.
        id
        (let ((root (disjoint-sets-find sets parent)))
          ;; TODO Don't do the path compression here... I feel like
          ;; it's going to add unecesary step to the egraph's hot
          ;; loop.
          ;; Path compression: point directly to the root if it's not
          ;; already the case.
          (when (/= root parent)
            (setf (aref sets id) root))
          root))))

(defun disjoint-sets-union (sets id1 id2)
  "Merge two disjoint sets. Return the set representative (the root)

Example:

(disjoint-sets-union sets 1 2)
=> 4 ; SETS is modified.
"
  (let ((root1 (disjoint-sets-find sets id1))
        (root2 (disjoint-sets-find sets id2)))
    (setf (aref sets root2) root1)))

(defun disjoint-sets-same-set-p (sets id1 id2)
  "Test if 2 items are in the same set.

Example:

(disjoint-sets-same-set-p sets 1 2)
=> T or NIL"
  (= (disjoint-sets-find sets id1)
     (disjoint-sets-find sets id2)))



(defun make-enode-map ()
  "Make an empty map from enodes to eclass ids"
  (make-hash-table :test 'equalp))

(defclass eclass ()
  ((id
    :initform (error "eclass: ID must be specified.")
    :initarg :id
    :accessor id
    :documentation "The id (integer) of the equivalenceclass.")
   (enodes
    :initform (make-array '(0) :adjustable t :fill-pointer t)
    :initarg :enodes
    :accessor enodes
    :documentation "The set of enodes that are equivalent.")
   (parents
    :initform (make-enode-map)
    :initarg :parents
    :accessor parents
    :documentation
    "Back-pointer to the parent enodes. Use for repairing the egraph's
invariants."))
  (:documentation "An equivalence class"))

(defun make-eclass (id enodes &optional parents)
  (make-instance
   'eclass
   :id id
   :enodes (make-array (list (length enodes))
                       :adjustable t
                       :fill-pointer t
                       :initial-contents enodes)
   :parents (or parents (make-enode-map))))

(defun eclass-add-enode (eclass enode)
  "Add an ENODE to an ECLASS."
  (vector-push-extend enode (enodes eclass)))

(defun add-parent (eclass parent-enode parent-eclass-id)
  "Add the PARENT-ENODE to ECLASS's parents"
  (setf (gethash parent-enode (parents eclass)) parent-eclass-id))

(defmethod print-object ((eclass eclass) stream)
  "Print ECLASS to STREAM."
  (print-unreadable-object
      (eclass stream :type t :identity t)
    (if (= 1 (length (enodes eclass)))
        (format stream "~d (1 enode: ~s, ~d parent enodes)"
                (id eclass)
                (aref (enodes eclass) 0)
                (hash-table-count (parents eclass)))
        (format stream "~d (~d enodes, ~d parent enodes)"
                (id eclass)
                (length (enodes eclass))
                (hash-table-count (parents eclass))))))



(defclass egraph ()
  ((enode-eclasses ;; enode -> eclass id
    :initform (make-hash-table :test 'equalp)
    :accessor enode-eclasses
    :documentation "A mapping from enode objects to eclass IDs.")
   (eclasses
    :initform (make-hash-table)
    :initarg :eclasses
    :accessor eclasses
    :documentation "Eclasses by their IDs.")
   (union-find
    :initform (make-disjoint-sets)
    :initarg :union-find
    :accessor union-find
    :documentation
    "A union-find data structure, that keeps track of equivalences between
eclasses.")
   (pending
    :initform (list)
    :initarg :pending
    :accessor pending
    :documentation "A list of pending eclass ids to fix their invariants."))
  (:documentation "An equivalence graph"))

(defmethod print-object ((egraph egraph) stream)
  "Print EGRAPH to STREAM."
  (print-unreadable-object
      (egraph stream :type t :identity t)
    (format stream "(~d e-nodes across ~d (~d) e-classes; ~d pending repair)"
            (hash-table-count (enode-eclasses egraph))
            (hash-table-count (eclasses egraph))
            (length (union-find egraph))
            (length (pending egraph)))))

(defun make-egraph ()
  "Create an empty egraph (equivalence graph)."
  (make-instance 'egraph))



(defun eclass (egraph eclass-id)
  "Get an eclass in EGRAPH from its ECLASS-ID."
  (gethash eclass-id (eclasses egraph)))

(defun (setf eclass) (eclass egraph eclass-id)
  "Add an ECLASS to EGRAPH, indexed by its ECLASS-ID."
  (setf (gethash eclass-id (eclasses egraph)) eclass))

(defun eclass-id (egraph enode)
  "Get an EGRAPH's ENODE's eclass-id."
  (gethash enode (enode-eclasses egraph)))

(defun (setf eclass-id) (eclass-id egraph enode)
  "Set an EGRAPH's ENODE's ECLASS-ID."
  (setf (gethash enode (enode-eclasses egraph)) eclass-id))

(defun eclass-find (egraph eclass-id)
  "Find the cannonical id for EGRAPH's eclass ECLASS-ID."
  (disjoint-sets-find (union-find egraph) eclass-id))



(defun cannonicalize (egraph enode)
  "Return ENODE with all its children's eclass-id replaced by the
cannonical (representative) eclass-id.

The first value is the original ENODE if it was already cannonical, or
a completely new enode if it wasn't.

The second value is NIL iif ENODE was already cannonical."
  (loop
    :with changed
    ;; TODO there's probably a way to postpone copy-seq until we know
    ;; it's really needed
    :with new-enode = (copy-seq enode)
    :for i :from 1 :below (length enode)
    :for eclass-id = (aref enode i)
    :for cannonical-eclass-id = (eclass-find egraph eclass-id)
    :unless (= eclass-id cannonical-eclass-id)
      :do (setf changed t
                (aref new-enode i) cannonical-eclass-id)
    :finally (return (values (if changed
                                 new-enode
                                 enode)
                             changed))))



(defun egraph-add-enode (egraph enode)
  "Add ENODE to EGRAPH, creating a new e-class if necessary."
  (or
   ;; Do nothing if the enode already exists in the egraph
   (eclass-id egraph enode)
   (let* (;; Allocate a new eclass-id
          (id (disjoint-sets-add (union-find egraph)))
          ;; Create a new eclass with that id
          (eclass (make-eclass id (list enode))))
     ;; Set the ENODE's eclass-id too
     (setf (eclass-id egraph enode) id)
     ;; Add the new eclass into the egraph
     (setf (eclass egraph id) eclass)
     ;; For each children of ENODE; add the new eclass as a parent.
     (when (vectorp enode)
       (loop
         :for i :from 1 :below (length enode)
         :for child-eclass-id := (aref enode i)
         :for child-eclass := (eclass egraph child-eclass-id)
         :do (add-parent child-eclass enode id)))
     id)))


(defun merge-eclass (egraph id1 id2)
  "Merge eclasses represented by ID1 and ID2.
This breaks the invariant of the egraph, rebuild must be called
aftewards to restore them."
  (let ((cannonical-id1 (eclass-find egraph id1))
        (cannonical-id2 (eclass-find egraph id2)))
    (if (= cannonical-id1 cannonical-id2)
        cannonical-id1
        (let ((new-cannonical-id
                (disjoint-sets-union (union-find egraph) id1 id2)))
          (push new-cannonical-id (pending egraph))
          new-cannonical-id))))

(defmethod repair-parent-enodes (egraph eclass)
  "Make sure each enodes is cannonical and points to a cannonical
eclass."
  (loop
    :for parent-enode
      :being :the :hash-key :of (parents eclass)
        :using (hash-value parent-eclass-id)
    :for new-enode = (cannonicalize egraph parent-enode)
    ;; Make sure the eclass ids contained in the enode are all
    ;; cannonical eclass ids
    :unless (eq new-enode parent-enode)
      :do (remhash parent-enode (enode-eclasses egraph))
    :do
       ;; Make sure the enode points to a cannonical eclass id
       (setf (eclass-id egraph new-enode)
             (eclass-find egraph parent-eclass-id))))

(defun repair-congruence (egraph eclass)
  "Restore the \"congruence\" invariant.

If you cannonicalize an enode and it has a new eclass, it means that
the new eclass is equivalent to the old one, and they must be merged.
"
  (loop
    :with new-parent-enodes = (make-enode-map)
    :for parent-enode
      :being :the :hash-key :of (parents eclass)
        :using (hash-value parent-eclass-id)
    :for new-enode = (cannonicalize egraph parent-enode)
    :for equivalent-eclass-id = (gethash new-enode new-parent-enodes)
    :do
       (when equivalent-eclass-id
         (merge-eclass egraph
                       parent-eclass-id
                       equivalent-eclass-id))
       (setf (gethash new-enode new-parent-enodes)
             (eclass-find egraph parent-eclass-id))
    :finally (setf (parents eclass) new-parent-enodes)))

(defun repair (egraph eclass-id)
  "Repair 1 eclass after it was merged with another."
  (let ((eclass (eclass egraph eclass-id)))
    (repair-parent-enodes egraph eclass)
    (repair-congruence egraph eclass)))

(defun to-set (sequence)
  "Utility to transform SEQUENCE to a hash-table where each key is also
the value.
It is used to de-duplicate SEQUENCE's elements."
  (let ((set (make-hash-table)))
    (map nil (lambda (element)
               (setf (gethash element set) element))
         sequence)
    set))

(defun rebuild (egraph)
  "Restore all EGRAPH's invariants.
It must be called after merging eclasses, but it is possible to do
many merges in a batch and only call rebuild once afterwards."
  (loop
    :while (pending egraph)
    :do
       (let ((todo (to-set (mapcar (lambda (eclass-id)
                                     (eclass-find egraph eclass-id))
                                   (pending egraph)))))
         (setf (pending egraph) nil)
         (loop
           :for eclass-id :being :the :hash-key :of todo
           :do (repair egraph eclass-id)))))



(defun %root-eclasses (egraph)
  "Find all eclasses that have no parents."
  (loop
    :for eclass-id :being
      :the :hash-key :of (eclasses egraph)
        :using (hash-value eclass)
    :when (zerop (hash-table-count (parents eclass)))
      :collect eclass))

(defun root-eclasses (egraph)
  "Find all the cannonical eclasses that have no parents."
  (let* ((roots (%root-eclasses egraph))
         (root-ids (to-set (mapcar #'id roots))))
    (loop
      :for eclass-id :being
        :the :hash-key :of (eclasses egraph)
          :using (hash-value eclass)
      :for cannonical-id = (eclass-find egraph eclass-id)
      :when (gethash cannonical-id root-ids)
        :collect eclass)))

(defun enode< (a b)
  (unless (eq a b)
    (cond
      ((eq a b) nil)
      ;; if one is a vector, but not the other
      ((and (not (vectorp a)) (vectorp b)) t)
      ((and (vectorp a) (not (vectorp b))) nil)
      ;; if a and b are symbols
      ((and (symbolp a) (symbolp b)) (string< a b))
      ;; if one is a number, but not the other
      ((and (not (numberp a)) (numberp b)) nil)
      ((and (numberp a) (not (numberp b))) t)
      ;; if they're both numbers
      ((and (numberp a) (numberp b)) (< a b))
      ;; if they're both vectors:
      ((and (vectorp a) (vectorp b))
       (loop
         :for i :from 0
         :for el1 :across a
         :for el2 :across b
         :unless (zerop i)
           :do (cond
                 ;; _could_ recurse here
                 ((< el1 el2) (return t))
                 ((> el1 el2) (return nil)))
         :finally
            ;; if we get there it's because either the 2 vectors are equal, or
            ;; one is the prefix of the other.
            ;;
            ;; So we return true, if a is shorter or has the same length than
            ;; b.
            (< (length a) (length b)))))))

(defun smallest-enodes (eclasses)
  "Find the smallest root enode (enode without parents)."
  (let* ((all-nodes (sort (apply #'concatenate 'vector
                                 (mapcar #'enodes eclasses))
                          #'enode<))
         (shortest (aref all-nodes 0)))
    ;; It's possible to have many enodes with the same size, so we
    ;; filter out all those that are bigger than the smallest.
    (remove-if (lambda (enode)
                 (enode< shortest enode))
               all-nodes)))



(defmethod add-form (egraph (form cons))
  "Add a FORM to an e-graph, creating e-classes if necessary."
  (egraph-add-enode
   egraph
   ;; Convert FROM into an enode.
   (apply #'vector
          (car form)
          (mapcar
           (lambda (element) (add-form egraph element))
           (rest form)))))
