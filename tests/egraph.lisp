(defpackage #:breeze.test.egraph
  (:documentation "Tests for the package breeze.egraph.")
  (:use #:cl #:breeze.egraph)
  (:import-from #:parachute
                #:define-test+run
                #:define-test
                #:is
                #:true
                #:false
                #:fail))

(in-package #:breeze.test.egraph)

(define-test+run eclass
  (let ((eclass (make-eclass 42 '(x))))
    (is = 42 (id eclass))
    (is equalp #(x) (enodes eclass)))
  (let ((eclass (make-eclass 43 '(a b c))))
    (is = 43 (id eclass))
    (is equalp #(a b c) (enodes eclass)))
  (let ((eclass (make-eclass 44 '(x) 'y)))
    (is = 44 (id eclass))
    (is equalp #(x) (enodes eclass))
    ;; N.B. we use the symbol Y, but really "parents" is supposed to
    ;; be a hash-table.
    (is eq 'y (parents eclass))))

(define-test+run "add enode(s) to egraph"
  (let* ((egraph (make-egraph))
         (enode 'x)
         ;; Adding the e-node to the e-graph
         (id (egraph-add-enode egraph enode))
         ;; Looking up the newly created e-class by the e-node
         (eclass (eclass egraph id)))
    ;; The first e-class we add should have the id 0
    (is = 0 id)
    ;; Verifying that the newly created e-class contains the e-node
    (is eq enode (aref (enodes eclass) 0))
    ;; Verifying the e-node's e-class
    (is = id (eclass-id egraph enode)))
  ;; Here, we add the e-node 'x to an e-graph that already contains it.
  (let ((egraph (make-egraph)))
    (egraph-add-enode egraph 'x)
    (let ((id (egraph-add-enode egraph 'x)))
      ;; The first e-class we add should have the id 0
      (is = 0 id)
      (is = 1 (length (union-find egraph)))
      (is = 1 (hash-table-count (eclasses egraph)))
      (is = 1 (hash-table-count (eclasses egraph)))))
  ;; Here, we add the same _FORM_ twice
  (let* ((egraph (make-egraph)))
    (add-form egraph '(+ 1 2))
    (add-form egraph '(+ 1 2))
    (is = 3 (length (union-find egraph)))
    (is = 3 (hash-table-count (eclasses egraph)))
    (is = 3 (hash-table-count (eclasses egraph))))
  (let* ((egraph (make-egraph)))
    (add-form egraph '(+ x y))
    (add-form egraph '(+ x 2))
    (add-form egraph '(+ y y))
    ;; 3 distinct forms + 3 disctinct atoms = 6
    (is = 6 (length (union-find egraph)))
    (is = 6 (hash-table-count (eclasses egraph)))
    (is = 6 (hash-table-count (eclasses egraph)))))



(define-test+run "enode<"
  ;; eq
  (progn
    (false (enode< #1=#() #1#))
    (false (enode< #2=1 #2#))
    (false (enode< 'x 'x)))
  ;; malformed enodes
  (progn
    (false (enode< #() #()))
    (false (enode< #(x) #(y)))
    (false (enode< #(x) #(x)))
    (false (enode< #(y) #(x))))
  ;; proper enodes with children
  (progn
    (false (enode< #() #()))
    (true (enode< #(x 0) #(y 1)))
    (false (enode< #(x 0) #(x 0)))
    (false (enode< #(y 1) #(x 0))))
  ;; symbols
  (progn
    (true (enode< 'x 'y))
    (false (enode< 'y 'x)))
  ;; symbols v.s. vectors
  (progn
    (true (enode< 'x #()))
    (false (enode< #() 'x)))
  ;; symbols v.s. numbers
  (progn
    (true (enode< 0 'x))
    (false (enode< 'x 0)))
  ;; vectors v.s. numbers
  (progn
    (true (enode< 0 #()))
    (false (enode< #() 0)))
  ;; numbers v.s. numbers
  (progn
    (true (enode< 0 1))
    (false (enode< 0 0))))

(defun dump-enodes (egraph)
  "Dump EGRAPH's enodes as a normalized list for inspection and
comparison."
  (sort
   (loop
     :for enode :being :the :hash-key :of (enode-eclasses egraph)
       :using (hash-value eclass-id)
     :collect (list :enode (if (vectorp enode)
                               (copy-seq enode)
                               enode)
                    :eclass-id eclass-id))
   #'enode<
   :key #'second))

(defun dump-eclass (egraph eclass &aux (eclass-id (id eclass)))
  "Dump EGRAPH's ECLASS as a list for inspection and comparison."
  `(:eclass-id ,eclass-id
    :enodes ,(copy-seq (enodes eclass))
    ,@(when (plusp (hash-table-count (parents eclass)))
        (list :parents (sort (alexandria:hash-table-values (parents eclass))
                             #'<)))
    ,@(let ((canonical-id (eclass-find egraph eclass-id)))
        (unless (= eclass-id canonical-id)
          (list := canonical-id)))))

(defun dump-eclasses (egraph)
  "Dump EGRAPH's eclasses as a list for inspection and comparison."
  (sort
   (loop
     :for eclass-id :being :the :hash-key :of (eclasses egraph)
       :using (hash-value eclass)
     :collect (dump-eclass egraph eclass))
   #'<
   :key #'second))

(defun dump-egraph (egraph)
  "Dump EGRPAH as a list for inspection and comparison."
  `(,@(when (plusp (hash-table-count (eclasses egraph)))
        (list :enodes (dump-enodes egraph)))
    ,@(when (plusp (hash-table-count (eclasses egraph)))
        (list :eclasses (dump-eclasses egraph)))
    ,@(when (pending egraph)
        (list :pending (pending egraph)))))



(define-test+run "add enode(s) - snapshot tests"
  (let* ((egraph (make-egraph)))
    (is equalp
        '()
        (dump-egraph egraph)))
  (let* ((egraph (make-egraph)))
    (egraph-add-enode egraph 'x)
    (is equalp
        '(:enodes ((:enode x :eclass-id 0))
          :eclasses ((:eclass-id 0 :enodes #(x))))
        (dump-egraph egraph)))
  ;; Here, we add the e-node 'x twice
  (let ((egraph (make-egraph)))
    (egraph-add-enode egraph 'x)
    (egraph-add-enode egraph 'x)
    (is equalp
        '(:enodes ((:enode x :eclass-id 0))
          :eclasses ((:eclass-id 0 :enodes #(x))))
        (dump-egraph egraph)))
  ;; Here, we add the same _FORM_ twice
  (let* ((egraph (make-egraph)))
    (add-form egraph '(+ 1 2))
    (add-form egraph '(+ 1 2))
    (is equalp
        '(:enodes ((:enode 1 :eclass-id 0)
                   (:enode 2 :eclass-id 1)
                   (:enode #(+ 0 1) :eclass-id 2))
          :eclasses ((:eclass-id 0 :enodes #(1) :parents (2))
                     (:eclass-id 1 :enodes #(2) :parents (2))
                     (:eclass-id 2 :enodes #(#(+ 0 1)))))
        (dump-egraph egraph)))
  (let ((egraph (make-egraph)))
    (add-form egraph '(+ x y))
    (add-form egraph '(+ x 2))
    (add-form egraph '(+ y y))
    (is equalp
        '(:enodes ((:enode 2 :eclass-id 3)
                   (:enode x :eclass-id 0)
                   (:enode y :eclass-id 1)
                   (:enode #(+ 0 1) :eclass-id 2)
                   (:enode #(+ 0 3) :eclass-id 4)
                   (:enode #(+ 1 1) :eclass-id 5))
          :eclasses ((:eclass-id 0 :enodes #(x) :parents (2 4))
                     (:eclass-id 1 :enodes #(y) :parents (2 5))
                     (:eclass-id 2 :enodes #(#(+ 0 1)))
                     (:eclass-id 3 :enodes #(2) :parents (4))
                     (:eclass-id 4 :enodes #(#(+ 0 3)))
                     (:eclass-id 5 :enodes #(#(+ 1 1)))))
        (dump-egraph egraph)))
  (let ((egraph (make-egraph)))
    (add-form egraph '(/ (* a 2) 2))
    (is equalp
        '(:enodes ((:enode 2 :eclass-id 1)
                   (:enode a :eclass-id 0)
                   (:enode #(* 0 1) :eclass-id 2)
                   (:enode #(/ 2 1) :eclass-id 3))
          :eclasses ((:eclass-id 0 :enodes #(a) :parents (2))
                     (:eclass-id 1 :enodes #(2) :parents (2 3))
                     (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
                     (:eclass-id 3 :enodes #(#(/ 2 1)))))
        (dump-egraph egraph))))

(define-test+run "add enode(s) - snapshot tests - step by step - (+ x y)"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is equalp ,expected (dump-egraph egraph)
                      ,when)))
      (check "after initialization" '())
      (add-form egraph 'x)
      (check "after adding the form 'x"
             '(:enodes ((:enode x :eclass-id 0))
               :eclasses ((:eclass-id 0 :enodes #(x)))))
      (add-form egraph 'y)
      (check "after adding the form 'y"
             '(:enodes ((:enode x :eclass-id 0)
                        (:enode y :eclass-id 1))
               :eclasses ((:eclass-id 0 :enodes #(x))
                          (:eclass-id 1 :enodes #(y)))))
      (add-form egraph '(+ x y))
      (check
       "after adding the form '(+ x y)"
       '(:enodes ((:enode x :eclass-id 0)
                  (:enode y :eclass-id 1)
                  (:enode #(+ 0 1) :eclass-id 2))
         :eclasses ((:eclass-id 0 :enodes #(x) :parents (2))
                    (:eclass-id 1 :enodes #(y) :parents (2))
                    (:eclass-id 2 :enodes #(#(+ 0 1)))))))))

(define-test+run "add enode(s) - snapshot tests - step by step - x is equivalent to y"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is equalp ,expected (dump-egraph egraph)
                      ,when)))
      (check "after initialization" '())
      (add-form egraph 'x)
      (check "after adding the form 'x"
             '(:enodes ((:enode x :eclass-id 0))
               :eclasses ((:eclass-id 0 :enodes #(x)))))
      (add-form egraph 'y)
      (check "after adding the form 'y"
             '(:enodes ((:enode x :eclass-id 0)
                        (:enode y :eclass-id 1))
               :eclasses ((:eclass-id 0 :enodes #(x))
                          (:eclass-id 1 :enodes #(y)))))
      ;; TODO maybe add a convenience method "merge-forms"
      (merge-eclass egraph
                    (eclass-id egraph 'x)
                    (eclass-id egraph 'y))
      (check "after merging the e-classes for the enodes 'x and 'y"
             '(:enodes ((:enode x :eclass-id 0)
                        (:enode y :eclass-id 1))
               :eclasses ((:eclass-id 0 :enodes #(x))
                          (:eclass-id 1 :enodes #(y) := 0))
               :pending (0)))
      (rebuild egraph)
      (check "after rebuild"
             ;; TODO This is technically correct (AFAIU), but it would
             ;; be nice to catch the cases where we merge eclasses
             ;; that represents only 1 form.
             '(:enodes ((:enode x :eclass-id 0)
                        (:enode y :eclass-id 1))
               :eclasses ((:eclass-id 0 :enodes #(x))
                          (:eclass-id 1 :enodes #(y) := 0)))))))

(define-test+run "add enode(s) - snapshot tests - 1 + 1 = 2"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is equalp ,expected (dump-egraph egraph)
                      ,when)))
      (merge-eclass egraph
                    (add-form egraph '2)
                    (prog1 (add-form egraph '(+ 1 1))
                      (check "before merging the e-classes for the enodes '2 and '(+ 1 1)"
                             '(:enodes ((:enode 1 :eclass-id 1)
                                        (:enode 2 :eclass-id 0)
                                        (:enode #(+ 1 1) :eclass-id 2))
                               :eclasses ((:eclass-id 0 :enodes #(2))
                                          (:eclass-id 1 :enodes #(1) :parents (2))
                                          (:eclass-id 2 :enodes #(#(+ 1 1)))))) ))
      (check "after merging the e-classes for the enodes '2 and '(+ 1 1)"
             '(:enodes ((:enode 1 :eclass-id 1)
                        (:enode 2 :eclass-id 0)
                        (:enode #(+ 1 1) :eclass-id 2))
               :eclasses ((:eclass-id 0 :enodes #(2))
                          (:eclass-id 1 :enodes #(1) :parents (2))
                          (:eclass-id 2 :enodes #(#(+ 1 1)) := 0))
               :pending (0)))
      (rebuild egraph)
      (check "after rebuild"
             '(:enodes ((:enode 1 :eclass-id 1)
                        (:enode 2 :eclass-id 0)
                        (:enode #(+ 1 1) :eclass-id 2))
               :eclasses ((:eclass-id 0 :enodes #(2))
                          (:eclass-id 1 :enodes #(1) :parents (2))
                          (:eclass-id 2 :enodes #(#(+ 1 1)) := 0)))))))

(define-test+run "add enode(s) - snapshot tests - a = a * 2 /2"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is equalp ,expected (dump-egraph egraph)
                      ,when))
               (check-add (form expected)
                 `(progn
                    (add-form egraph ,form)
                    (check ,(format nil "after adding ~(~s~)" form)
                           ,expected)))
               (check-merge (form1 form2 expected)
                 `(progn
                    (merge-eclass egraph
                                  (add-form egraph ,form1)
                                  (add-form egraph ,form2))
                    (check ,(format nil "after merging ~(~s and ~s~)"
                                    form1 form2)
                           ,expected))))
      (check-add
       '(/ (* a 2) 2)
       '(:enodes ((:enode 2 :eclass-id 1)
                  (:enode a :eclass-id 0)
                  (:enode #(* 0 1) :eclass-id 2)
                  (:enode #(/ 2 1) :eclass-id 3))
         :eclasses ((:eclass-id 0 :enodes #(a) :parents (2))
                    (:eclass-id 1 :enodes #(2) :parents (2 3))
                    (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
                    (:eclass-id 3 :enodes #(#(/ 2 1))))))
      (check-merge
       '(* a 2)
       '(ash a 1)
       '(:enodes ((:enode 1 :eclass-id 4)
                  (:enode 2 :eclass-id 1)
                  (:enode a :eclass-id 0)
                  (:enode #(* 0 1) :eclass-id 2)
                  (:enode #(ash 0 4) :eclass-id 5)
                  (:enode #(/ 2 1) :eclass-id 3))
         :eclasses ((:eclass-id 0 :enodes #(a) :parents (2 5))
                    (:eclass-id 1 :enodes #(2) :parents (2 3))
                    (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
                    (:eclass-id 3 :enodes #(#(/ 2 1)))
                    (:eclass-id 4 :enodes #(1) :parents (5))
                    (:eclass-id 5 :enodes #(#(ash 0 4)) := 2))
         :pending (2)))
      (check-merge
       '(/ (* a 2) 2)
       '(* a (/ 2 2))
       '(:enodes ((:enode 1 :eclass-id 4)
                  (:enode 2 :eclass-id 1)
                  (:enode a :eclass-id 0)
                  (:enode #(* 0 1) :eclass-id 2)
                  (:enode #(ash 0 4) :eclass-id 5)
                  (:enode #(* 0 6) :eclass-id 7)
                  (:enode #(/ 1 1) :eclass-id 6)
                  (:enode #(/ 2 1) :eclass-id 3))
         :eclasses ((:eclass-id 0 :enodes #(a) :parents (2 5 7))
                    (:eclass-id 1 :enodes #(2) :parents (2 3 6))
                    (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
                    (:eclass-id 3 :enodes #(#(/ 2 1)))
                    (:eclass-id 4 :enodes #(1) :parents (5))
                    (:eclass-id 5 :enodes #(#(ash 0 4)) := 2)
                    (:eclass-id 6 :enodes #(#(/ 1 1)) :parents (7))
                    (:eclass-id 7 :enodes #(#(* 0 6)) := 3))
         :pending (3 2)))
      (check-merge
       '(/ 2 2)
       1
       '(:enodes ((:enode 1 :eclass-id 4)
                  (:enode 2 :eclass-id 1)
                  (:enode a :eclass-id 0)
                  (:enode #(* 0 1) :eclass-id 2)
                  (:enode #(ash 0 4) :eclass-id 5)
                  (:enode #(* 0 6) :eclass-id 7)
                  (:enode #(/ 1 1) :eclass-id 6)
                  (:enode #(/ 2 1) :eclass-id 3))
         :eclasses ((:eclass-id 0 :enodes #(a) :parents (2 5 7))
                    (:eclass-id 1 :enodes #(2) :parents (2 3 6))
                    (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
                    (:eclass-id 3 :enodes #(#(/ 2 1)))
                    (:eclass-id 4 :enodes #(1) :parents (5) := 6)
                    (:eclass-id 5 :enodes #(#(ash 0 4)) := 2)
                    (:eclass-id 6 :enodes #(#(/ 1 1)) :parents (7))
                    (:eclass-id 7 :enodes #(#(* 0 6)) := 3))
         :pending (6 3 2)))
      (rebuild egraph)
      (check "after rebuild"
             '(:enodes ((:enode 1 :eclass-id 4)
                        (:enode 2 :eclass-id 1)
                        (:enode a :eclass-id 0)
                        (:enode #(* 0 1) :eclass-id 2)
                        (:enode #(ash 0 4) :eclass-id 5)
                        (:enode #(* 0 6) :eclass-id 3)
                        (:enode #(/ 1 1) :eclass-id 6)
                        (:enode #(/ 2 1) :eclass-id 3))
               :eclasses ((:eclass-id 0 :enodes #(a) :parents (2 5 7))
                          (:eclass-id 1 :enodes #(2) :parents (2 3 6))
                          (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
                          (:eclass-id 3 :enodes #(#(/ 2 1)))
                          (:eclass-id 4 :enodes #(1) :parents (5) := 6)
                          (:eclass-id 5 :enodes #(#(ash 0 4)) := 2)
                          (:eclass-id 6 :enodes #(#(/ 1 1)) :parents (3))
                          (:eclass-id 7 :enodes #(#(* 0 6)) := 3)))))))


(define-test+run "can I extract something useful?"
  (let ((egraph (make-egraph))
        (input '(/ (* a 2) 2)))
    (labels ((add* (form)
               (add-form egraph form))
             (merge* (form1 form2)
               (merge-eclass egraph (add* form1) (add* form2)))
             (dump-eclass* (eclass)
               (dump-eclass egraph eclass)))
      (add* input)
      (merge* '(* a 2)
              '(ash a 1))
      (merge* '(/ (* a 2) 2)
              '(* a (/ 2 2)))
      (merge* '(/ 2 2)
              1)
      (merge* '(* a 1)
              'a)
      (rebuild egraph)
      (is equalp
          '(:enodes ((:enode 1 :eclass-id 4)
                     (:enode 2 :eclass-id 1)
                     (:enode a :eclass-id 0)
                     (:enode #(* 0 1) :eclass-id 2)
                     (:enode #(ash 0 4) :eclass-id 5)
                     (:enode #(* 0 4) :eclass-id 8)
                     (:enode #(/ 1 1) :eclass-id 6)
                     (:enode #(/ 2 1) :eclass-id 3)
                     (:enode #(* 8 6) :eclass-id 3))
            :eclasses ((:eclass-id 0 :enodes #(a) :parents (2 5 7 8) := 8)
                       (:eclass-id 1 :enodes #(2) :parents (2 3 6))
                       (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
                       (:eclass-id 3 :enodes #(#(/ 2 1)))
                       (:eclass-id 4 :enodes #(1) :parents (5 8) := 6)
                       (:eclass-id 5 :enodes #(#(ash 0 4)) := 2)
                       (:eclass-id 6 :enodes #(#(/ 1 1)) :parents (3))
                       (:eclass-id 7 :enodes #(#(* 0 6)) := 3)
                       (:eclass-id 8 :enodes #(#(* 0 4)))))
          ;; (add* input) = 3
          (dump-egraph egraph))
      ;; Finding the "root eclasses"
      (is equalp
          '((:eclass-id 3 :enodes #(#(/ 2 1)))
            (:eclass-id 5 :enodes #(#(ash 0 4)) := 2)
            (:eclass-id 7 :enodes #(#(* 0 6)) := 3)
            (:eclass-id 8 :enodes #(#(* 0 4))))
          (loop
            :for eclass-id :being
              :the :hash-key :of (eclasses egraph)
                :using (hash-value eclass)
            :when (zerop (hash-table-count (parents eclass)))
              :collect (dump-eclass egraph eclass))
          "when trying to find the roots")
      (is equalp
          '((:eclass-id 0 :enodes #(a) :parents (2 5 7 8) := 8)
            (:eclass-id 3 :enodes #(#(/ 2 1)))
            (:eclass-id 7 :enodes #(#(* 0 6)) := 3)
            (:eclass-id 8 :enodes #(#(* 0 4))))
          (mapcar #'dump-eclass* (root-eclasses egraph))
          "when trying to find the roots and their closure")
      ;; Victory!
      (is equalp
          #(a)
          (smallest-enodes
           (root-eclasses egraph))))))



;;; Work in Progress - ematching!

;; TODO "rules" would be "bidirectional" and "rewrites" wouldn't.
;; TODO (defun rule (a b) ...)
;; TODO (defun make-rewrite (antecedant consequent) ...)

(defun mkrule (lhs rhs)
  (let ((breeze.pattern::*term-pool* (make-hash-table)))
    (cons (breeze.pattern:compile-pattern lhs)
          (breeze.pattern:compile-pattern rhs))))

#++
(mkrule '(/ ?x ?x) 1)

#++
(mkrule '(/ (* ?x ?y) ?z)
        '(* ?x (/ ?y ?z)))

#++
(mkrule '(/ ?x 1) ?x)


#++ ;; TODO make a test out of this
;; This checks that all terms have the same address
;; a more portable way would be ~ every + eq
(map 'list #'sb-kernel:get-lisp-obj-address
     (breeze.pattern:compile-pattern '(?x ?x)))

(defun match-enode (egraph enode pattern set-of-bindings)
  ;; TODO support for variable-length matches
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
      :finally (return new-set-of-bindings))))

(defun match-eclass (egraph eclass pattern &optional (set-of-bindings '(t)))
  (check-type egraph breeze.egraph::egraph)
  (check-type eclass breeze.egraph::eclass)
  (etypecase pattern
    (breeze.pattern:term
     ;; The whole class "matches"
     (list (breeze.pattern::make-binding pattern eclass)))
    ((or vector symbol number)
     ;; Find every enode that matches the pattern
     (loop :for enode :across (enodes eclass)
           :for new-set-of-bindings := (and (vectorp enode)
                                            (match-enode egraph enode pattern set-of-bindings))
           :when new-set-of-bindings
             :append new-set-of-bindings))))

#++
(progn
  #++
  (trace match-enode match-eclass
         merge-sets-of-bindings
         breeze.pattern::merge-bindings
         breeze.pattern::make-bindings)

  (let ((egraph (make-egraph))
        (input '(/ (* a 2) 2)))
    (labels ((add* (form)
               (add-form egraph form))
             (merge* (form1 form2)
               (merge-eclass egraph (add* form1) (add* form2)))
             (dump-eclass* (eclass)
               (dump-eclass egraph eclass)))
      (add* input)
      #++
      (progn
        (merge* '(* a 2)
                '(ash a 1))
        (merge* '(/ (* a 2) 2)
                '(* a (/ 2 2)))
        (merge* '(/ 2 2)
                1)
        (merge* '(* a 1)
                'a))
      (rebuild egraph))
    (let ((rules
            (list
             (mkrule '(/ (* ?x ?y) ?y) '?x) ; this one should match
             )
            #++
            (list
             (mkrule '(/ ?x ?x) 1) ; unless ?x == 0
             (mkrule '(* (/ ?x ?y) ?y) '?x)

             (mkrule '(/ (* ?x ?y) ?z)
                     '(* ?x (/ ?y ?z)))
             (mkrule '(/ ?x 1) '?x)
             (mkrule '(* ?x 1) '?x)
             (mkrule '(* 1 ?x) '?x)
             (mkrule '(/ ?x 0) 0))))
      (loop :for (pattern . substitution) :in rules
            :collect
            (loop
              :for eclass :in (root-eclasses egraph)
              :for set-of-bindings = (match-eclass egraph eclass pattern)
              :collect (list :pattern pattern
                             :match set-of-bindings
                             :substitutions (mapcar (lambda (bindings)
                                                      (pattern-substitute substitution bindings))
                                                    set-of-bindings)))))
    ;; (dump-egraph egraph)
    ))


(defun make-egraph* (input &rest other-inputs)
  (let ((egraph (make-egraph)))
    (add-form egraph input)
    (map nil (lambda (i) (add-form egraph i)) other-inputs)
    (rebuild egraph)
    egraph))

(defun match-rewrite (egraph rewrite)
  "Match 1 rewrite against an egraph, returns a list of bindings."
  (loop
    :with (pattern . substitution) := rewrite
    :for eclass :in (root-eclasses egraph)
    :for set-of-bindings = (match-eclass egraph eclass pattern)
    :collect (list eclass
                   ;; Compute the substitutions
                   (mapcar (lambda (bindings)
                             (pattern-substitute substitution bindings))
                           set-of-bindings))))

(defun apply-rewrite (egraph rewrite)
  "Match REWRITE's pattern against EGRAPH. Add the new forms and merge
the corresponding ECLASSES.
Does NOT rebuild the egraph's invariants."
  (loop :for (eclass forms) :in (match-rewrite egraph rewrite)
        :do (loop :for new-form :in forms
                  :for eclass-id = (add-form egraph new-form)
                  :do (merge-eclass egraph (id eclass) eclass-id)))
  egraph)

(define-test+run "apply 1 rewrite"
  (is equalp #(a)
      (let ((egraph (make-egraph* '(/ (* a 2) 2)))
            (rewrite (mkrule '(/ (* ?x ?y) ?y) '?x)))
        (apply-rewrite egraph rewrite)
        (rebuild egraph)
        (smallest-enodes
         (root-eclasses egraph)))))

#++ ;; TODO It would be nice to be able to add a form as a vector into
;; an egraph. I think it could help with performance, because
;; applying a rule and adding the resulting "substituted" form
;; would not involve conversion between lists and vectors anymore.
(let ((egraph (make-egraph)))
  (add-form egraph #(/ #(* a 2) 2))
  (dump-egraph egraph))
