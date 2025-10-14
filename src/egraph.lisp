(uiop:define-package #:breeze.egraph
    (:documentation "Equivalence graphs to compactly represent lots of code.

- egraphs ≅ set of eclasses
- eclasses ≅ set of enodes
- enode ≅ an operator + operands eclasses (not enodes)")
  (:use #:cl)
  (:use-reexport #:breeze.pattern)
  ;; Eclass
  (:export
   #:make-eclass
   #:id
   #:enodes
   #:parents)
  ;; Egraph
  (:export
   #:egraph
   #:make-egraph
   #:enode-eclasses
   #:eclasses
   #:union-find
   #:eclasses
   #:pending
   #:input-eclasses)
  (:export
   #:eclass
   #:eclass-id
   #:eclass-find
   #:canonicalize
   #:egraph-add-enode
   #:merge-eclass
   #:rebuild)
  (:export
   #:root-node-p
   #:root-eclass-p
   #:root-eclasses
   #:enode<
   #:smallest-enodes)
  (:export
   #:add-form
   #:add-input)
  (:export
   #:match-rewrite
   #:apply-rewrite))

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
          ;; it's going to add unnecessary step to the egraph's hot
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
    "Back-pointer to the parent enodes. Used for repairing the egraph's
invariants.
Keys are parent enodes
Values are the parent enode's eclass-id
"))
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
  (format stream "#<eclass ~d ~a>" (id eclass) (enodes eclass))
  #++ (print-unreadable-object
          (eclass stream :type t :identity nil)
        (format stream "~3d" (id eclass))
        (format stream " ~w" (enodes eclass))
        ;; (format stream "~{~w~}" (enodes eclass))
        #++
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
    :documentation "A list of pending eclass ids to fix their invariants.")
   (input-eclasses
    :initform (list)
    :accessor input-eclasses
    :documentation "A convenient list of eclass ids considered as \"inputs\"."))
  (:documentation "An equivalence graph"))


(defmethod print-object ((egraph egraph) stream)
  "Print EGRAPH to STREAM."
  (print-unreadable-object
      (egraph stream :type t :identity nil)
    (format stream "~d e-nodes across ~d e-classes, ~d pending repairs"
            (hash-table-count (enode-eclasses egraph))
            (hash-table-count (eclasses egraph))
            ;; (length (union-find egraph))
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
  "Find the canonical id for EGRAPH's eclass ECLASS-ID."
  (disjoint-sets-find (union-find egraph) eclass-id))

(defmethod enodes ((egraph egraph))
  "Get all the enodes in EGRAPH as a list."
  (alexandria:hash-table-keys (enode-eclasses egraph)))

(defun enode-parents (egraph enode)
  "Returns a list of parent enodes."
  (alexandria:hash-table-keys
   (parents
    (eclass egraph (eclass-id egraph enode)))))

(defun root-eclass-p (eclass)
  "Is eclass a root eclass?"
  (zerop (hash-table-count (parents eclass))))

(defun root-enode-p (egraph enode)
  (zerop
   (hash-table-count
    (parents
     (eclass egraph (eclass-id egraph enode))))))



(defun canonicalize (egraph enode)
  "Return ENODE with all its children's eclass-id replaced by the
canonical (representative) eclass-id.

The first value is the original ENODE if it was already canonical, or
a completely new enode if it wasn't.

The second value is NIL iif ENODE was already canonical."
  (loop
    :with changed
    ;; TODO there's probably a way to postpone copy-seq until we know
    ;; it's really needed
    :with new-enode = (copy-seq enode)
    :for i :from 1 :below (length enode)
    :for eclass-id = (aref enode i)
    :for canonical-eclass-id = (eclass-find egraph eclass-id)
    :unless (= eclass-id canonical-eclass-id)
      :do (setf changed t
                (aref new-enode i) canonical-eclass-id)
    :finally (return (values (if changed
                                 new-enode
                                 enode)
                             changed))))



(defun enode-add-parent (egraph parent-eclass-id enode)
  "Go through the ENODE's children eclass and add PARENT-ECLASS-ID as its parent."
  ;; For each children of ENODE; add the new eclass as a parent.
  (when (vectorp enode)
    (loop
      :for i :from 1 :below (length enode)
      :for child-eclass-id := (aref enode i)
      #++ (let ((id (aref enode i)))
            (if (typep id 'eclass) (id id) id))
      :for child-eclass := (eclass egraph child-eclass-id)
      :do (add-parent child-eclass enode parent-eclass-id))))

(defun egraph-add-enode (egraph enode)
  "Add ENODE to EGRAPH, creating a new e-class if necessary. Returns the
eclass-id (and a second value T, if the eclass was just created)."
  (or
   ;; Do nothing if the enode already exists in the egraph
   (when (typep enode 'eclass)
     (values (id enode) t))
   (alexandria:when-let ((eclass-id (eclass-id egraph enode)))
     (values eclass-id t))
   (let* (;; Allocate a new eclass-id
          (id (disjoint-sets-add (union-find egraph)))
          ;; Create a new eclass with that id
          (eclass (make-eclass id (list enode))))
     ;; Set the ENODE's eclass-id too
     (setf (eclass-id egraph enode) id)
     ;; Add the new eclass into the egraph
     (setf (eclass egraph id) eclass)
     ;; For each children of ENODE; add the new eclass as a parent.
     (enode-add-parent egraph id enode)
     ;; Return the new eclass-id
     (values id nil))))

(defun merge-eclass (egraph id1 id2)
  "Merge eclasses represented by ID1 and ID2.
This breaks the invariant of the egraph, rebuild must be called
afterwards to restore them."
  (let ((canonical-id1 (eclass-find egraph id1))
        (canonical-id2 (eclass-find egraph id2)))
    (if (= canonical-id1 canonical-id2)
        canonical-id1
        (let ((new-canonical-id
                (disjoint-sets-union (union-find egraph) id1 id2)))
          (push new-canonical-id (pending egraph))
          (let* ((eclass1 (eclass egraph id1))
                 (eclass2 (eclass egraph id2))
                 (parents1 (parents eclass1))
                 (parents2 (parents eclass2)))
            (unless (eq parents1 parents2)
              ;; now they share the same table
              (setf (parents eclass2) parents1)
              ;; TODO add parents2 to eclass1's parents
              (maphash (lambda (parent-enode parent-eclass-id)
                         (add-parent eclass1 parent-enode parent-eclass-id))
                       parents2)))
          new-canonical-id))))

(defmethod repair-parent-enodes (egraph eclass)
  "Make sure each enodes is canonical and points to a canonical
eclass."
  (loop
    :for parent-enode
      :being :the :hash-key :of (parents eclass)
        :using (hash-value parent-eclass-id)
    :for new-enode = (canonicalize egraph parent-enode)
    ;; Make sure the eclass ids contained in the enode are all
    ;; canonical eclass ids
    :unless (eq new-enode parent-enode)
      :do (remhash parent-enode (enode-eclasses egraph))
    :do
       ;; Make sure the enode points to a canonical eclass id
       (setf (eclass-id egraph new-enode)
             (eclass-find egraph parent-eclass-id))))

(defun repair-congruence (egraph eclass)
  "Restore the \"congruence\" invariant.

If you canonicalize an enode and it has a new eclass, it means that
the new eclass is equivalent to the old one, and they must be merged.
"
  (loop
    :with new-parent-enodes = (make-enode-map)
    :for parent-enode
      :being :the :hash-key :of (parents eclass)
        :using (hash-value parent-eclass-id)
    :for new-enode = (canonicalize egraph parent-enode)
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
           :do (repair egraph eclass-id))))
  egraph)



(defun root-eclasses (egraph)
  "Find all eclasses that have no parents."
  (loop
    :for eclass-id :being
      :the :hash-key :of (eclasses egraph)
        :using (hash-value eclass)
    :when (root-eclass-p eclass)
      :collect eclass))

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
         :do (cond
               ((equalp el1 el2) nil)
               ((enode< el1 el2) (return t))
               (t (return nil)))
         :finally
            ;; if we get there it's because either the 2 vectors are equal, or
            ;; one is the prefix of the other.
            ;;
            ;; So we return true, if a is shorter than b.
            (< (length a) (length b)))))))

(defun smallest-enodes (eclasses)
  "Find the smallest enode from a set of eclasses."
  (let* ((all-nodes (sort (apply #'concatenate 'vector
                                 (mapcar #'enodes eclasses))
                          #'enode<))
         (shortest (aref all-nodes 0)))
    ;; It's possible to have many enodes with the same size, so we
    ;; filter out all those that are bigger than the smallest.
    (remove-if (lambda (enode)
                 (enode< shortest enode))
               all-nodes)))



(defmethod add-form (egraph form)
  "Add a FORM to an e-graph, creating e-classes if necessary. Returns an
eclass-id."
  (egraph-add-enode egraph form))

(defmethod add-form (egraph (form cons))
  "Add a FORM to an e-graph, creating e-classes if necessary. Returns an
eclass-id."
  (egraph-add-enode
   egraph
   ;; Convert FORM into an enode.
   (apply #'vector
          (car form)
          (mapcar
           (lambda (element) (add-form egraph element))
           (rest form)))))

;; This one is used to add "forms" that were created by pattern
;; substitution (e.g. by a rewrite). The pattern matching codes works
;; with vectors.
(defmethod add-form (egraph (form vector))
  "Add a FORM to an e-graph, creating e-classes if necessary. Returns an
eclass-id."
  (egraph-add-enode
   egraph
   ;; Convert FORM into an enode.
   (apply #'vector
          (aref form 0)
          (loop :for i :from 1 :below (length form)
                :collect (add-form egraph (aref form i))))
   #++ ;; TODO don't treat the first element in a special way
   (map 'vector
        (lambda (element) (add-form egraph element))
        form)))

;; This is also required for adding forms created by pattern
;; substitution, because the matching and substitution is done on
;; enodes, it creates forms that contains instance of eclasses (the
;; bindings during matching binds to instances of elcasses).
(defmethod add-form (egraph (eclass eclass))
  "Add ECLASS to an e-graph, creating e-classes if necessary."
  ;; assumes it's already in the egraph
  (id eclass))

(defmethod add-input (egraph form)
  (let ((eclass-id (add-form egraph form)))
    (pushnew eclass-id (input-eclasses egraph))))


;;; E-matching, rewrites, rules, etc.

(defun match-enode (egraph enode pattern set-of-bindings)
  ;; TODO support for variable-length matches
  (etypecase enode
    ((or symbol number)
     (merge-sets-of-bindings
      set-of-bindings
      (list (match pattern enode))))
    (vector
     (when (alexandria:length= enode pattern)
       ;; This is getting very complicated because
       ;; "match-eclass" returns a list of possible bindings...
       (loop
         :for eclass-id :across enode
         :for subpattern :across pattern
         :for new-set-of-bindings =
         ;; TODO Optimization: check if match returns T
         ;; Probably not worth it, as I may change that code altogether...
                                  (merge-sets-of-bindings
                                   set-of-bindings
                                   ;; TODO This assumes that the first element of the enode is not an eclass
                                   (list (breeze.pattern::match eclass-id subpattern)))
           :then (merge-sets-of-bindings
                  new-set-of-bindings
                  (match-eclass egraph
                                (eclass egraph eclass-id)
                                subpattern new-set-of-bindings))
         :while new-set-of-bindings
         :finally (return new-set-of-bindings))))))

(defun match-eclass (egraph eclass pattern &optional (set-of-bindings '(t)))
  (check-type egraph egraph)
  (check-type eclass eclass)
  (etypecase pattern
    (breeze.pattern:var
     ;; The whole class "matches"
     (list (make-binding (name pattern) eclass)))
    ((or vector symbol number)
     ;; Find every enode that matches the pattern
     (loop :for enode :across (enodes eclass)
           :for new-set-of-bindings := (match-enode egraph enode pattern set-of-bindings)
           :when new-set-of-bindings
             :append new-set-of-bindings))))

(defun match-rewrite (egraph rewrite)
  "Match 1 rewrite against an egraph, returns a list of substituted
forms."
  (loop
    :with pattern := (rewrite-pattern rewrite)
    :with substitution := (rewrite-template rewrite)
    :for eclass-id :being :the :hash-key :of (eclasses egraph)
      :using (hash-value eclass)
    ;;    :for eclass :in (eclasses egraph)
    :for set-of-bindings = (match-eclass egraph eclass pattern)
    :when set-of-bindings
      :collect (list eclass
                     ;; Compute the substitutions
                     (mapcar (lambda (bindings)
                               (pattern-substitute substitution bindings))
                             set-of-bindings))))

(defun apply-rewrite (egraph rewrite)
  "Match REWRITE's pattern against EGRAPH. Add the new forms and merge
the corresponding ECLASSES.
Does NOT rebuild the egraph's invariants."
  #++ (format t "~&Rewrite from ~s to ~s"
              (rewrite-pattern rewrite)
              (rewrite-template rewrite))
  (loop :for (eclass forms) :in (match-rewrite egraph rewrite)
        :do (loop :for new-form :in forms
                  :for new-eclass-id = (add-form egraph new-form)
                  :for new-eclass = (eclass egraph new-eclass-id)
                  :do (maphash (lambda (parent-enode parent-eclass-id)
                                 ;; (declare (ignore parent-enode))
                                 (add-parent new-eclass parent-enode parent-eclass-id))
                               (parents eclass))
                  :do (merge-eclass egraph (id eclass) new-eclass-id)))
  egraph)


;;; Stream/Iterators

(defgeneric stream-get (stream))

(defgeneric stream-next (stream))

(defgeneric stream-done-p (stream))

(defun collect (stream &key (limit))
  (if limit
      (loop
        repeat limit
        until (stream-done-p stream)
        collect (stream-get stream)
        do (stream-next stream))
      (loop
        until (stream-done-p stream)
        collect (stream-get stream)
        do (stream-next stream))))

(defun map-stream (fn stream &key (limit))
  (if limit
      (loop
        repeat limit
        until (stream-done-p stream)
        do (funcall fn (stream-get stream))
        do (stream-next stream))
      (loop
        until (stream-done-p stream)
        do (funcall fn (stream-get stream))
        do (stream-next stream))))


(defun stream-sequence (seq)
  "Create a stream out of a sequence, the stream will repeat the sequence
infinitely, but will be considered \"done\" as soon as the iterator is
past the last element of the sequence."
  (let ((i 0)
        (l (length seq)))
    (if (zerop l)
        (lambda (&optional method)
          (ecase method
            ((nil) nil)
            (:next (progn (incf i) 0))
            (:done t)))
        (lambda (&optional method)
          (ecase method
            ((nil) (elt seq (mod i l)))
            (:next (progn (incf i)
                          (if (zerop l)
                              0
                              (zerop (mod i l)))))
            (:done (<= l i)))))))

(defun stream-constant (x)
  "Create a stream out of one value."
  (stream-sequence (vector x)))

(defmethod stream-get ((stream function))
  (funcall stream))

(defmethod stream-next ((stream function))
  (funcall stream :next))

(defmethod stream-done-p ((stream function))
  (funcall stream :done))

;; TODO filter-stream

(defun next-list-of-stream (streams)
  "Given a sequence of streams, advance the sequence as a whole."
  (etypecase streams
    (cons (loop
            for s in streams
            for nextp = (stream-next s)
            while nextp
            finally (return nextp)))
    (vector (loop
              for s across streams
              for nextp = (stream-next s)
              while nextp
              finally (return nextp)))))


(defun stream-product (streams)
  "Create a stream that produces the Cartesian product of all the STREAMS."
  (let ((donep nil))
    (lambda (&optional method)
      (ecase method
        (:next (setf donep (next-list-of-stream streams)))
        (:done donep)
        ((nil) (map 'list 'stream-get streams))))))

(defun stream-sequence-product (sequences)
  "Create a stream that produces the Cartesian product of all the SEQUENCES."
  (stream-product
   (mapcar #'stream-sequence sequences)))

#++
(collect (stream-sequence-product '(#(a b) #(c d))))
;; => ((A C) (B C) (A D) (B D))

(defun stream-concat (streams)
  (if (zerop (length streams))
      (stream-sequence nil)
      (let* ((s (stream-sequence streams))
             (current (stream-get s)))
        (lambda (&optional method)
          (ecase method
            (:next (when (stream-next current)
                     (stream-next s)
                     (setf current (stream-get s))
                     (stream-done-p current)))
            (:done (stream-done-p current) (stream-done-p s))
            ((nil) (stream-get current)))))))

#++
(collect
    (stream-concat (vector (stream-sequence '(a b)) (stream-constant 'c))))
;; => (A B C)


;;; Streaming terms out of egraphs

(defun stream-equivalent-eclasses (egraph eclass-id)
  "Create a stream that will iterate over all the eclasses that are
equivalent to eclass-id."
  (let* ((canonical-id (eclass-find egraph eclass-id))
         (id -1)
         (union-find (union-find egraph))
         (l (length union-find)))
    (flet ((find-next ()
             ;; Find the next equivalent id in the union-find data
             ;; structure.
             (loop
               :for i = (incf id)
               :while (< id l)
               :for eclass-id = (aref union-find i)
               :when (= canonical-id eclass-id)
                 :do (return id))))
      (find-next)
      (lambda (&optional method)
        (ecase method
          (:next
           (find-next))
          (:done (<= l id))
          ((nil) id))))))

(defun %stream-eclass (egraph eclass)
  "Stream every enodes in the eclasses, one after the other."
  (stream-concat
   (map 'vector
        (lambda (enode) (stream-enode egraph enode))
        (enodes eclass))))

(defun stream-eclass (egraph eclass)
  "Stream every equivalent classes of eclass"
  (let* ((canonical-id (eclass-find egraph (id eclass)))
         (union-find (union-find egraph))
         (length (length union-find)))
    (stream-concat
     (map 'vector
          (lambda (eclass-id)
            (%stream-eclass egraph (eclass egraph eclass-id)))
          ;; Find all the equivalent id in the union-find data structure.
          (loop
            :for i :from 0
            :while (< i length)
            :for eclass-id = (aref union-find i)
            :when (= canonical-id eclass-id)
              :collect i)))))

(defun stream-enode (egraph enode)
  (etypecase enode
    ((or symbol number) (stream-constant enode))
    (vector (stream-product
             (let ((i 0))
               (map 'vector
                    (lambda (eclass-id-or-constant)
                      (cond
                        ((plusp i) (stream-eclass egraph (eclass egraph eclass-id-or-constant)))
                        (t (incf i) (stream-constant eclass-id-or-constant))))
                    enode))))))

(defun map-egraph (fn egraph &key limit #| TODO maybe add argument `eclasses-ids' and use that instead of (root-eclasses egraph) |#)
  (map-stream
   fn
   (stream-concat
    (map 'vector (lambda (eclass)
                   (stream-eclass egraph eclass))
         ;; TODO %root-eclasses is the right one to call BUT some
         ;; eclass'e parents are not set correctly during rewriting.
         ;; (breeze.egraph::%root-eclasses egraph)
         (root-eclasses egraph)))
   :limit limit))
