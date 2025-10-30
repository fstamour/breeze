(defpackage #:breeze.test.egraph
  (:documentation "Tests for the package breeze.egraph.")
  (:use #:cl #:breeze.egraph)
  (:import-from #:parachute
                #:define-test+run
                #:define-test
                #:is
                #:true
                #:false
                #:fail)
  (:import-from #:breeze.egraph
                #:map-stream
                #:map-egraph
                #:stream-eclass
                #:stream-equivalent-eclasses))

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
    ;; 3 distinct forms + 3 distinct atoms = 6
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
    (true (enode< #(x) #(y)))
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

(defun sort-enodes-dump (enodes-dump)
  (sort enodes-dump #'enode< :key #'second))

(defun dump-enodes (egraph)
  "Dump EGRAPH's enodes as a normalized list for inspection and
comparison."
  (sort-enodes-dump
   (loop
     :for enode :being :the :hash-key :of (enode-eclasses egraph)
       :using (hash-value eclass-id)
     :collect (list :enode (if (vectorp enode)
                               (copy-seq enode)
                               enode)
                    :eclass-id eclass-id))))

(defun dump-eclass (egraph eclass &aux (eclass-id (id eclass)))
  "Dump EGRAPH's ECLASS as a list for inspection and comparison."
  `(:eclass-id ,eclass-id
    :enodes ,(copy-seq (enodes eclass))
    ,@(if (plusp (hash-table-count (parents eclass)))
          (list :parents (sort (alexandria:hash-table-values (parents eclass))
                               #'enode<
                               #++ #'(lambda (a b)
                                       (when (and (numberp a))) <)))
          (list :root))
    ,@(let ((canonical-id (eclass-find egraph eclass-id)))
        (unless (= eclass-id canonical-id)
          (list := canonical-id)))))

(defun sort-eclasses-dump (eclasses-dump)
  (sort eclasses-dump #'< :key #'second))

(defun dump-eclasses (egraph)
  "Dump EGRAPH's eclasses as a list for inspection and comparison."
  (sort-eclasses-dump
   (loop
     :for eclass-id :being :the :hash-key :of (eclasses egraph)
       :using (hash-value eclass)
     :collect (dump-eclass egraph eclass))))

(defun dump-egraph (egraph)
  "Dump EGRPAH as a list for inspection and comparison."
  `(,@(when (plusp (hash-table-count (eclasses egraph)))
        (list :enodes (dump-enodes egraph)))
    ,@(when (plusp (hash-table-count (eclasses egraph)))
        (list :eclasses (dump-eclasses egraph)))
    ,@(when (pending egraph)
        (list :pending (pending egraph)))))

(defun normalize-egraph-dump (egraph-dump)
  (setf #1=(getf egraph-dump :enodes) (sort-enodes-dump #1#)
        #2=(getf egraph-dump :eclasses) (sort-eclasses-dump #2#)
        #| TODO maybe normalize "pending" |#))

(defun egraph-dumps-equal-p (egraph-dump1 egraph-dump2)
  (let ((egraph-dump1 (normalize-egraph-dump (copy-seq egraph-dump1)))
        (egraph-dump2 (normalize-egraph-dump (copy-seq egraph-dump2))))
    (equalp egraph-dump1 egraph-dump2)))



(define-test+run "add enode(s) - snapshot tests"
  (let* ((egraph (make-egraph)))
    (is egraph-dumps-equal-p
        '()
        (dump-egraph egraph)))
  (let* ((egraph (make-egraph)))
    (egraph-add-enode egraph 'x)
    (is egraph-dumps-equal-p
        '(:enodes ((:enode x :eclass-id 0))
          :eclasses ((:eclass-id 0 :enodes #(x) :root)))
        (dump-egraph egraph)))
  ;; Here, we add the e-node 'x twice
  (let ((egraph (make-egraph)))
    (egraph-add-enode egraph 'x)
    (egraph-add-enode egraph 'x)
    (is egraph-dumps-equal-p
        '(:enodes ((:enode x :eclass-id 0))
          :eclasses ((:eclass-id 0 :enodes #(x) :root)))
        (dump-egraph egraph)))
  ;; Here, we add the same _FORM_ twice
  (let* ((egraph (make-egraph)))
    (add-form egraph '(+ 1 2))
    (add-form egraph '(+ 1 2))
    (is egraph-dumps-equal-p
        '(:enodes ((:enode 1 :eclass-id 0)
                   (:enode 2 :eclass-id 1)
                   (:enode #(+ 0 1) :eclass-id 2))
          :eclasses ((:eclass-id 0 :enodes #(1) :parents (2))
                     (:eclass-id 1 :enodes #(2) :parents (2))
                     (:eclass-id 2 :enodes #(#(+ 0 1)) :root)))
        (dump-egraph egraph)))
  (let ((egraph (make-egraph)))
    (add-form egraph '(+ x y))
    (add-form egraph '(+ x 2))
    (add-form egraph '(+ y y))
    (is egraph-dumps-equal-p
        '(:enodes ((:enode 2 :eclass-id 3)
                   (:enode x :eclass-id 0)
                   (:enode y :eclass-id 1)
                   (:enode #(+ 0 1) :eclass-id 2)
                   (:enode #(+ 0 3) :eclass-id 4)
                   (:enode #(+ 1 1) :eclass-id 5))
          :eclasses ((:eclass-id 0 :enodes #(x) :parents (2 4))
                     (:eclass-id 1 :enodes #(y) :parents (2 5))
                     (:eclass-id 2 :enodes #(#(+ 0 1)) :root)
                     (:eclass-id 3 :enodes #(2) :parents (4))
                     (:eclass-id 4 :enodes #(#(+ 0 3)) :root)
                     (:eclass-id 5 :enodes #(#(+ 1 1)) :root)))
        (dump-egraph egraph)))
  (let ((egraph (make-egraph)))
    (add-form egraph '(/ (* a 2) 2))
    (is egraph-dumps-equal-p
        '(:enodes ((:enode 2 :eclass-id 1)
                   (:enode a :eclass-id 0)
                   (:enode #(* 0 1) :eclass-id 2)
                   (:enode #(/ 2 1) :eclass-id 3))
          :eclasses ((:eclass-id 0 :enodes #(a) :parents (2))
                     (:eclass-id 1 :enodes #(2) :parents (2 3))
                     (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
                     (:eclass-id 3 :enodes #(#(/ 2 1)) :root)))
        (dump-egraph egraph))))

(define-test+run "add enode(s) - snapshot tests - step by step - (+ x y)"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is egraph-dumps-equal-p ,expected (dump-egraph egraph)
                      ,when)))
      (check "after initialization" '())
      (add-form egraph 'x)
      (check "after adding the form 'x"
             '(:enodes ((:enode x :eclass-id 0))
               :eclasses ((:eclass-id 0 :enodes #(x) :root))))
      (add-form egraph 'y)
      (check "after adding the form 'y"
             '(:enodes ((:enode x :eclass-id 0)
                        (:enode y :eclass-id 1))
               :eclasses ((:eclass-id 0 :enodes #(x) :root)
                          (:eclass-id 1 :enodes #(y) :root))))
      (add-form egraph '(+ x y))
      (check
       "after adding the form '(+ x y)"
       '(:enodes ((:enode x :eclass-id 0)
                  (:enode y :eclass-id 1)
                  (:enode #(+ 0 1) :eclass-id 2))
         :eclasses ((:eclass-id 0 :enodes #(x) :parents (2))
                    (:eclass-id 1 :enodes #(y) :parents (2))
                    (:eclass-id 2 :enodes #(#(+ 0 1)) :root)))))))

(define-test+run "add enode(s) - snapshot tests - step by step - x is equivalent to y"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is egraph-dumps-equal-p ,expected (dump-egraph egraph)
                      ,when)))
      (check "after initialization" '())
      (add-form egraph 'x)
      (check "after adding the form 'x"
             '(:enodes ((:enode x :eclass-id 0))
               :eclasses ((:eclass-id 0 :enodes #(x) :root))))
      (add-form egraph 'y)
      (check "after adding the form 'y"
             '(:enodes ((:enode x :eclass-id 0)
                        (:enode y :eclass-id 1))
               :eclasses ((:eclass-id 0 :enodes #(x) :root)
                          (:eclass-id 1 :enodes #(y) :root))))
      ;; TODO maybe add a convenience method "merge-forms"
      (merge-eclass egraph
                    (eclass-id egraph 'x)
                    (eclass-id egraph 'y))
      (check "after merging the e-classes for the enodes 'x and 'y"
             '(:enodes ((:enode x :eclass-id 0)
                        (:enode y :eclass-id 1))
               :eclasses ((:eclass-id 0 :enodes #(x) :root)
                          (:eclass-id 1 :enodes #(y) :root := 0))
               :pending (0)))
      (rebuild egraph)
      (check "after rebuild"
             ;; TODO This is technically correct (AFAIU), but it would
             ;; be nice to catch the cases where we merge eclasses
             ;; that represents only 1 form.
             '(:enodes ((:enode x :eclass-id 0)
                        (:enode y :eclass-id 1))
               :eclasses ((:eclass-id 0 :enodes #(x) :root)
                          (:eclass-id 1 :enodes #(y) :root := 0)))))))

#|

TODO add a test to show that redundant eclasses are removed
1. add-form (+ a 2)
2. add-form (+ b 2)
3. assert a == b

check that there's only one eclass to represent both (+ a 2) and (+ b
2), and that it has only 1 enode (+ <eclass {a,b}> <eclass {2}>)

|#


(define-test+run "add enode(s) - snapshot tests - 1 + 1 = 2"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is egraph-dumps-equal-p ,expected (dump-egraph egraph)
                      ,when)))
      (merge-eclass egraph
                    (add-form egraph '2)
                    (prog1 (add-form egraph '(+ 1 1))
                      (check "before merging the e-classes for the enodes '2 and '(+ 1 1)"
                             '(:enodes ((:enode 1 :eclass-id 1)
                                        (:enode 2 :eclass-id 0)
                                        (:enode #(+ 1 1) :eclass-id 2))
                               :eclasses ((:eclass-id 0 :enodes #(2) :root)
                                          (:eclass-id 1 :enodes #(1) :parents (2))
                                          (:eclass-id 2 :enodes #(#(+ 1 1)) :root))))))
      (check "after merging the e-classes for the enodes '2 and '(+ 1 1)"
             '(:enodes ((:enode 1 :eclass-id 1)
                        (:enode 2 :eclass-id 0)
                        (:enode #(+ 1 1) :eclass-id 2))
               :eclasses ((:eclass-id 0 :enodes #(2) :root)
                          (:eclass-id 1 :enodes #(1) :parents (2))
                          (:eclass-id 2 :enodes #(#(+ 1 1)) :root := 0))
               :pending (0)))
      (rebuild egraph)
      (check "after rebuild"
             '(:enodes ((:enode 1 :eclass-id 1)
                        (:enode 2 :eclass-id 0)
                        (:enode #(+ 1 1) :eclass-id 2))
               :eclasses ((:eclass-id 0 :enodes #(2) :root)
                          (:eclass-id 1 :enodes #(1) :parents (2))
                          (:eclass-id 2 :enodes #(#(+ 1 1)) :root := 0)))))))


;; TODO add 2; add (+ (+ 1 1) 1); assert 2 = (+ 1 1) then eclass for
;; the value "2" should have the same parent as all the equivalent
;; classes. Perhaphs only keep track of the parents in the class
;; representative?
(define-test+run "add enode(s) - snapshot tests - 1 + 1 = 2 & 3 + (1 + 1)"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is egraph-dumps-equal-p ,expected (dump-egraph egraph)
                      ,when)))
      (let* ((e2 (add-form egraph '2))
             (e1+1 (add-form egraph '(+ 1 1)))
             (e3+ (add-form egraph '(+ 3 (+ 1 1)))))
        (declare (ignorable e3+))
        (check "before merging the e-classes for the enodes '2 and '(+ 1 1)"
               `(:enodes
                 ;; enodes for the value 1, 2 and 3 happen to have the
                 ;; eclass-id 1, 2 and 3.
                 #1=((:enode 1 :eclass-id 1)
                     (:enode 2 :eclass-id ,e2)
                     (:enode 3 :eclass-id 3)
                     (:enode #(+ 1 1) :eclass-id ,e1+1)
                     (:enode #(+ 3 2) :eclass-id ,e3+))
                 :eclasses
                 ((:eclass-id 0 :enodes #(2) :root)
                  (:eclass-id 1 :enodes #(1)        :parents (2))
                  (:eclass-id 2 :enodes #(#(+ 1 1)) :parents (,e3+))
                  (:eclass-id 3 :enodes #(3)        :parents (,e3+))
                  (:eclass-id 4 :enodes #(#(+ 3 2)) :root))))
        (merge-eclass egraph e2 e1+1)
        (check "after merging the e-classes for the enodes '2 and '(+ 1 1)"
               `(:enodes #1#
                 :eclasses
                 ((:eclass-id ,e2   :enodes #(2)        :parents (,e3+))
                  (:eclass-id 1     :enodes #(1)        :parents (2))
                  (:eclass-id ,e1+1 :enodes #(#(+ 1 1)) :parents (,e3+)  := ,e2)
                  (:eclass-id 3     :enodes #(3)        :parents (,e3+))
                  (:eclass-id ,e3+  :enodes #(#(+ 3 2)) :root))
                 :pending (0)))
        (rebuild egraph)
        (check "after rebuild"
               `(:enodes #1#
                 :eclasses
                 ((:eclass-id ,e2   :enodes #(2)        :parents (,e3+))
                  (:eclass-id 1     :enodes #(1)        :parents (2))
                  (:eclass-id ,e1+1 :enodes #(#(+ 1 1)) :parents (,e3+)  := ,e2)
                  (:eclass-id 3     :enodes #(3)        :parents (,e3+))
                  (:eclass-id ,e3+  :enodes #(#(+ 3 2)) :root))))))))

(define-test+run "add enode(s) - snapshot tests - a = a * 2 /2"
  (let ((egraph (make-egraph)))
    (macrolet ((check (when expected)
                 `(is egraph-dumps-equal-p ,expected (dump-egraph egraph)
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
                    (:eclass-id 3 :enodes #(#(/ 2 1)) :root))))
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
                    (:eclass-id 3 :enodes #(#(/ 2 1)) :root)
                    (:eclass-id 4 :enodes #(1) :parents (5))
                    (:eclass-id 5 :enodes #(#(ash 0 4)) :parents (3) := 2))
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
                    (:eclass-id 3 :enodes #(#(/ 2 1)) :root)
                    (:eclass-id 4 :enodes #(1) :parents (5))
                    (:eclass-id 5 :enodes #(#(ash 0 4)) :parents (3) := 2)
                    (:eclass-id 6 :enodes #(#(/ 1 1)) :parents (7))
                    (:eclass-id 7 :enodes #(#(* 0 6)) :root := 3))
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
                    (:eclass-id 3 :enodes #(#(/ 2 1)) :root)
                    (:eclass-id 4 :enodes #(1) :parents (5 7) := 6)
                    (:eclass-id 5 :enodes #(#(ash 0 4)) :parents (3) := 2)
                    (:eclass-id 6 :enodes #(#(/ 1 1)) :parents (5 7))
                    (:eclass-id 7 :enodes #(#(* 0 6)) :root := 3))
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
                          (:eclass-id 3 :enodes #(#(/ 2 1)) :root)
                          (:eclass-id 4 :enodes #(1) :parents (5 7) := 6)
                          (:eclass-id 5 :enodes #(#(ash 0 4)) :parents (3) := 2)
                          (:eclass-id 6 :enodes #(#(/ 1 1)) :parents (2 3))
                          (:eclass-id 7 :enodes #(#(* 0 6)) :root := 3)))))))


(define-test+run "can I extract something useful?"
  (let ((egraph (make-egraph))
        (input '(/ (* a 2) 2)))
    (labels ((add* (form)
               (add-form egraph form))
             (merge* (form1 form2)
               (merge-eclass egraph (add* form1) (add* form2)))
             ;; file: /home/fstamour/dev/breeze/tests/egraph.lisp
             ;; in: DEFINE-TEST+RUN "can I extract something useful?"
             ;;     (BREEZE.TEST.EGRAPH::DUMP-ECLASS* (BREEZE.EGRAPH:ECLASS)
             ;;      (BREEZE.TEST.EGRAPH::DUMP-ECLASS BREEZE.EGRAPH:EGRAPH BREEZE.EGRAPH:ECLASS))
             ;;
             ;; note: deleting unused function
             ;;   (LABELS DUMP-ECLASS* :IN "/home/fstamour/dev/breeze/tests/egraph.lisp")
             (dump-eclass* (eclass)
               (dump-eclass egraph eclass)))
      (declare (ignorable (function dump-eclass*)))
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
      (is egraph-dumps-equal-p
          '(:enodes
            ((:enode 1 :eclass-id 4)
             (:enode 2 :eclass-id 1)
             (:enode a :eclass-id 0)
             (:enode #(* 8 1) :eclass-id 2)
             (:enode #(* 8 6) :eclass-id 8)
             (:enode #(/ 1 1) :eclass-id 6)
             (:enode #(/ 2 1) :eclass-id 8)
             (:enode #(ash 8 6) :eclass-id 2))
            :eclasses
            ((:eclass-id 0 :enodes #(a) :parents (2 5 7 8) := 8)
             (:eclass-id 1 :enodes #(2) :parents (2 3 6))
             (:eclass-id 2 :enodes #(#(* 0 1)) :parents (8))
             (:eclass-id 3 :enodes #(#(/ 2 1)) :parents (2 2 8) := 8)
             (:eclass-id 4 :enodes #(1) :parents (5 7 8) := 6)
             (:eclass-id 5 :enodes #(#(ash 0 4)) :parents (3) := 2)
             (:eclass-id 6 :enodes #(#(/ 1 1)) :parents (2 8))
             (:eclass-id 7 :enodes #(#(* 0 6)) :root := 8)
             (:eclass-id 8 :enodes #(#(* 0 4)) :parents (2 2 8))))
          ;; (add* input) = 3
          (dump-egraph egraph))
      ;; Finding the "root eclasses"
      (is equalp
          '((:eclass-id 7 :enodes #(#(* 0 6)) :root := 8))
          (loop
            :for eclass-id :being
              :the :hash-key :of (eclasses egraph)
                :using (hash-value eclass)
            :when (zerop (hash-table-count (parents eclass)))
              :collect (dump-eclass egraph eclass))
          "when trying to find the roots")
      ;; TODO The following next 2 tests were working because of a bug
      ;; with how the parents was tracked.
      #++
      (is equalp
          '((:eclass-id 0 :enodes #(a) :parents (2 5 7 8) := 8)
            (:eclass-id 3 :enodes #(#(/ 2 1)))
            (:eclass-id 7 :enodes #(#(* 0 6)) := 3)
            (:eclass-id 8 :enodes #(#(* 0 4))))
          (mapcar #'dump-eclass* (root-eclasses egraph))
          "when trying to find the roots and their closure")
      #++
      ;; Victory!
      (is equalp
          #(a)
          (smallest-enodes
           (root-eclasses egraph))))))



;;; Work in Progress - ematching!


(defun make-egraph* (input &rest other-inputs)
  (let ((egraph (make-egraph)))
    (add-input egraph input)
    (map nil (lambda (i) (add-form egraph i)) other-inputs)
    (rebuild egraph)
    egraph))



(defun ensure-egraph (input)
  (if (typep input 'egraph) input (make-egraph* input)))

(defun collect-eclass-forms (egraph eclass-id)
  "Return a list of all the forms represented by the e-class ECLASS-ID."
  (let (forms)
    (map-stream
     #'(lambda (form) (push form forms))
     (stream-eclass egraph (eclass egraph eclass-id)))
    forms))

(defun eclasses-forms (egraph eclass-ids)
  "Returns all the forms represented by the eclasses ECLASS-IDS in
EGRAPH."
  ;; for each eclasses, collect the forms it represents.
  (loop :for eclass-id :in eclass-ids
        :append (collect-eclass-forms egraph eclass-id)))

(defun dump-input-eclasses-forms (egraph)
  (eclasses-forms
   egraph
   ;; cannonicalize and de-duplicate the input eclasses
   (remove-duplicates
    (mapcar (lambda (eclass-id)
              (eclass-find egraph eclass-id))
            (input-eclasses egraph)))))

(defun test-rewrite (input rewrite
                     &optional expected-equivalent-forms)
  "Takes an INPUT form, tries to apply REWRITE to it."

  (let* ((egraph (ensure-egraph input))
         (before (dump-input-eclasses-forms egraph))
         (after (progn (apply-rewrite egraph rewrite)
                       (rebuild egraph)
                       (dump-input-eclasses-forms egraph))))
    (when expected-equivalent-forms
      (is equalp
          expected-equivalent-forms
          after
          "Applying the rewrite ~%~s to ~%~s was expected to result in the forms ~%~s, got ~%~s instead"
          rewrite before expected-equivalent-forms after))
    egraph))

(defun test-rewrite-chain (input &rest rewrites-and-expected-forms)
  (loop
    :for (rewrite expected-forms) :on rewrites-and-expected-forms
    :by #'cddr
    :for egraph := (test-rewrite input rewrite expected-forms)
      :then (test-rewrite egraph rewrite expected-forms)))

(define-test+run rewrites
  ;; This should test that "stream-eclass" handles cycle correctly
  ;; (i.e. without recursing infinitely)
  (test-rewrite '(* 0 a)
                (make-rewrite '(* 0 ?x) 0)
                '((* 0 a)
                  0))
  (test-rewrite '(= 0 a)
                (make-rewrite '(= 0 ?x) '(zerop ?x))
                '((zerop a)
                  (= 0 a)))
  (test-rewrite '(/ a a)
                (make-rewrite '(/ ?x ?x) 1)
                '(1 (/ a a)))
  (test-rewrite '(/ a a)
                (make-rewrite '(/ ?x ?y) '(* ?x (/ 1 ?y)))
                '((* a (/ 1 a)) (/ a a)))
  (test-rewrite '(* a (/ 1 a))
                (make-rewrite '(* ?x (/ 1 ?y)) '(/ ?x ?y))
                '((/ a a) (* a (/ 1 a))))
  ;; TODO this rewrites produce a cyclic e-graph, which causes an
  ;; infite recursion when dumping all the forms reprented by the
  ;; egraph.
  #++
  (test-rewrite '(* a (/ 1 a))
                (make-rewrite '(* ?x (/ 1 ?y)) 1)
                '((* a (/ 1 a)) 1))
  (test-rewrite '(* a b)
                (make-rewrite '(* ?x ?y) '(* ?y ?x))
                '((* b a) (* a b)))
  (test-rewrite '(+ a b c)
                (make-rewrite '(+ ?x ?y ?z) '(+ ?x (+ ?y ?z)))
                '((+ a (+ b c)) (+ a b c)))
  (test-rewrite '(* a 2)
                (make-rewrite '(* ?x 2) '(ash ?x 1))
                '((ash a 1) (* a 2)))
  ;; TODO these rewrites produce a cyclic e-graph, which causes an
  ;; infite recursion when dumping all the forms reprented by the
  ;; egraph.
  #++
  (test-rewrite-chain
   '(/ a a)
   (make-rewrite '(/ ?x ?x) 1)
   '(1 (/ a a))
   (make-rewrite '(/ ?x ?y) '(* ?x (/ 1 ?y))))
  (test-rewrite-chain
   '(+ a b c)
   (make-rewrite '(+ ?x ?y ?z) '(+ ?x (+ ?y ?z)))
   '((+ a (+ b c)) (+ a b c))
   (make-rewrite '(+ ?x ?y ?z) '(+ (+ ?x ?y) ?z))
   '((+ (+ a b) c) (+ a (+ b c)) (+ a b c))
   (make-rewrite '(+ ?x ?y) '(+ ?y ?x))
   '((+ c (+ b a)) (+ c (+ a b))
     (+ (+ c b) a) (+ (+ b c) a)
     (+ (+ b a) c) (+ (+ a b) c)
     (+ a (+ c b)) (+ a (+ b c))
     (+ a b c)))
  ;; This one doesn't work because the code to dump the forms doens't
  ;; look at equivalent classes (except for the "root" (the "input
  ;; eclasses")).
  (test-rewrite-chain
   '(+ 1 (* a 2))
   (make-rewrite '(* ?x 2) '(ash ?x 1))
   '((+ 1 (ash a 1)) (+ 1 (* a 2)))))

#++
(trace :wherein test-rewrite-chain
       match
       breeze.egraph::match-rewrite
       breeze.egraph::match-eclass
       breeze.egraph::match-enode)



(defun test-simple-rewrite (input pattern template)
  "Takes an INPUT form, match it against PATTERN, if successful, use the resulting bindings to fill TEMPLATE.

This is for interactive use, it logs a loooot of stuff."
  (test-simple-rewrite* input (make-rewrite pattern template)))

(defun test-simple-rewrite* (input rewrite)
  "Takes an INPUT form, tries to apply REWRITE to it.

This is for interactive use, it logs a loooot of stuff."
  (format t "~%~%")
  (let ((egraph (if (typep input 'egraph) input (make-egraph* input))))
    (map-egraph #'print egraph :limit 100)
    (format t "~%~%")
    (let ((before (dump-egraph egraph))
          (after (progn (apply-rewrite egraph rewrite)
                        (rebuild egraph)
                        (dump-egraph egraph))))
      (progn
        (format t "~%~%")
        (format t "~&Applying the rewrite rule:~&    ~s~&    ~s"
                (rewrite-pattern rewrite)
                (rewrite-template rewrite))
        (format t "~%~%")
        (format t "~&Enodes before:~%~{    ~s~^~%~}" (second before))
        (format t "~&Enodes after :~%~{    ~s~^~%~}" (second after))
        (format t "~%~%")
        (format t "~&Eclasses before:~%~{    ~s~^~%~}" (fourth before))
        ;; (format t "~&Eclasses after :~%~{    ~s~^~%~}" (fourth after))
        (format t "~&Eclasses after:")
        (dolist (eclass-ish (fourth after))
          (format t "~&    ~s's forms:" eclass-ish)
          (let ((eclass-id (second eclass-ish)))
            (map-stream #'(lambda (form)
                            (format t "~&        ~a" form))
                        (stream-eclass egraph (eclass egraph eclass-id)))))
        (format t "~%~%")
        (loop
          :for input-eclass-id :in (input-eclasses egraph)
          :do
             (format t "~&Forms in input e-class ~d:" input-eclass-id)
             (map-stream #'(lambda (form)
                             (format t "~&-> ~a" form))
                         (stream-eclass egraph (eclass egraph input-eclass-id)))))
      #++
      (progn
        (format t "~&All the Forms in the egraph:")
        (map-egraph #'print egraph :limit 100))
      egraph)))

#++
(progn
 (untrace)
 (trace
    wherein test-simple-rewrite
  pattern-substitute
  breeze.egraph::match-rewrite
  breeze.egraph::match-eclass
  breeze.egraph::match-enode
  merge-eclass
  add-form
  breeze.egraph::egraph-add-enode
  breeze.egraph::form-to-enode
  breeze.egraph::sequence-to-enode
  breeze.egraph::atom-to-enode
  match
  ;; add-parent
  merge-sets-of-bindings))

#++
(let ((egraph (test-simple-rewrite '(+ 1 (* a 2)) '(* ?x 2) '(ash ?x 1))))
  ;; (test-simple-rewrite egraph '(+ ?x ?y) '(+ ?y ?x))
  ;; (setf *e* egraph)
  )

#++
(let ((egraph (test-simple-rewrite '(/ (* a 2) 2) '(/ (* ?x ?y) ?y) '?x)))
  (setf *e* egraph))

;; '(/ (* a 2) 2)
;; (untrace)


#|
Input:
(+ a b c)

Rewrites (applied in this order):
'(+ ?x ?y ?z) '(+ ?x (+ ?y ?z))
'(+ ?x ?y ?z) '(+ (+ ?x ?y) ?z)
'(+ ?x ?y) '(+ ?y ?x)

Forms represented by the egraph:
(+ A B C)
(+ A (+ B C))
(+ (+ A B) C)
(+ (+ B C) A)
(+ C (+ A B))
|#


(define-test+run "apply 1 rewrite"
  (is egraph-dumps-equal-p
      '(:enodes
        ((:enode 2 :eclass-id 1)
         (:enode a :eclass-id 0)
         (:enode #(* 3 1) :eclass-id 2)
         (:enode #(/ 2 1) :eclass-id 3))
        :eclasses
        ((:eclass-id 0 :enodes #(a) :parents (2) := 3)
         (:eclass-id 1 :enodes #(2) :parents (2 3))
         (:eclass-id 2 :enodes #(#(* 0 1)) :parents (3))
         (:eclass-id 3 :enodes #(#(/ 2 1)) :parents (2))))
      (let ((egraph (make-egraph* '(/ (* a 2) 2)))
            (rewrite (make-rewrite '(/ (* ?x ?y) ?y) '?x)))
        (apply-rewrite egraph rewrite)
        (dump-egraph (rebuild egraph))
        #++ ;; TODO
        (smallest-enodes
         (root-eclasses egraph)))))

#++
(let ((egraph (make-egraph)))
  (add-form egraph #(/ #(* a 2) 2))
  (dump-egraph egraph))

#++
(defparameter *e* nil)



#++
(let ((egraph (make-egraph* '(/ (* a 2) 2)))
      (*print-readably* nil)
      (*print-level* nil)
      (*print-length* nil))
  (format t "~&=========================================")
  (format t "~&=========================================")
  (format t "~&=========================================")
  (let ((rewrites
          (list
           ;; These are not all sounds
           (make-rewrite '(/ (* ?x ?y) ?y) '?x)
           (make-rewrite '(* (/ ?x ?y) ?y) '?x)

           (make-rewrite '(* ?x 2) '(ash ?x 1))

           (make-rewrite '(/ ?x ?x) 1)

           (make-rewrite '(/ (* ?x ?y) ?z) '(* ?x (/ ?y ?z)))

           ;; (make-rewrite '(/ ?x 1) '?x)
           ;; (make-rewrite '(* ?x 1) '?x)
           ;; (make-rewrite '(* 1 ?x) '?x)
           ;; (make-rewrite '(/ 0 ?x) 0)
           )))
    (loop :repeat 1
          :do
             (format t "~&=========================================")
             (loop :for rewrite :in rewrites
                   :do (test-simple-rewrite* egraph rewrite)
                   #++ (progn (apply-rewrite egraph rewrite)
                              (rebuild egraph)
                              (map-egraph #'print egraph :limit 100)))))
  egraph)

;;

#++
(progn
  (untrace)
  (dump-egraph *e*)
  (map-egraph #'print *e* :limit 100))


;; (= 0 ?x) => (zerop x)
;; (= x ?0) => (zerop x)
;; (and (zerop ?x (= ?x ?y))) => (= 0 ?x ?y)
