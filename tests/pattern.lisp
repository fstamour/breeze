(defpackage #:breeze.test.pattern
  (:documentation "Test package for breeze.pattern.")
  (:use #:cl #:breeze.pattern #:breeze.iterator)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:is-values
                #:isnt
                #:true
                #:false
                #:of-type
                #:fail
                #:finish)
  ;; importing non-exported symbols
  (:import-from #:breeze.pattern
                #:term-symbol-p
                #:any-symbol-p
                #:bindings
                #:make-binding-set
                #:copy-binding-set
                #:set-binding
                #:add-binding
                #:emptyp))

(in-package #:breeze.test.pattern)

(define-test+run any
  (let ((any (any)))
    (of-type any any)
    (true (anyp any))))

(define-test+run term
  (let ((term (term :x)))
    (of-type term term)
    (true (termp term))
    (is eq :x (name term))))

(define-test+run term=
  (true (term= (term :x) (term :x)))
  (false (term= (term :y) (term :x)))
  (false (term= (term :y) 42)))

(define-test+run maybe
  (let ((maybe (maybe :x)))
    (of-type repetition maybe)
    (is eq :x (pattern maybe))
    (is = 0 (minimum maybe))
    (is = 1 (maximum maybe))))

(define-test+run zero-or-more
  (let ((zero-or-more (zero-or-more :x)))
    (of-type repetition zero-or-more)
    (is eq :x (pattern zero-or-more))
    (is = 0 (minimum zero-or-more)
        "A pattern created with the function `zero-or-more' should have a minimum of 0.")
    (false (maximum zero-or-more)
           "A pattern created with the function `zero-or-more' should not have a maximum.")))

(define-test+run alternation
  (let ((alternation (alternation #(:x))))
    (of-type alternation alternation)
    (true (alternationp alternation))
    (is equalp #(:x) (patterns alternation))))

;; TODO alternation=

(define-test+run pattern=
  (is pattern= 'x 'x)
  (is pattern= '(x) '(x))
  (is pattern= (term 'x) (term 'x))
  (is pattern= (maybe 'y) (maybe 'y))
  (is pattern= (maybe '(x y)) (maybe '(x y)))
  ;; TODO Maybe I should try to detect this case when compiling...
  (is pattern= (maybe (maybe 'x)) (maybe (maybe 'x)))
  (is pattern= (zero-or-more 'y) (zero-or-more 'y))
  (is pattern= (zero-or-more '(x y)) (zero-or-more '(x y)))
  (is pattern= (zero-or-more (maybe 'x)) (zero-or-more (maybe 'x)))
  (is pattern= (alternation #(y)) (alternation #(y)))
  (is pattern= (alternation #(x y)) (alternation #(x y)))
  (is pattern=
      (alternation (vector (maybe 'x)))
      (alternation (vector (maybe 'x)))))



(define-test+run term-symbol-p
  (true (term-symbol-p :?x))
  (false (term-symbol-p 'x))
  (false (term-symbol-p "?x")))

(define-test+run any-symbol-p
  (true (any-symbol-p :_x))
  (false (any-symbol-p 'x))
  (false (any-symbol-p "_x")))

(define-test+run compile-pattern
  (is pattern= (any) (compile-pattern :_x))
  (is pattern= :x (compile-pattern :x))
  (is pattern= 42 (compile-pattern 42))
  (is pattern= (term :?x) (compile-pattern :?x))
  (is pattern= (maybe :x) (compile-pattern '(:maybe :x)))
  (is pattern= (maybe #(:x :y)) (compile-pattern '(:maybe (:x :y))))
  (is pattern= (zero-or-more #(:x)) (compile-pattern '(:zero-or-more :x)))
  (is pattern= (zero-or-more #(:x :y)) (compile-pattern '(:zero-or-more :x :y)))
  (is pattern= (alternation #(:x)) (compile-pattern '(:alternation :x)))
  (is pattern= (alternation #(:x :y)) (compile-pattern '(:alternation :x :y)))
  (multiple-value-bind (p terms)
      (compile-pattern '(?x ?x))
    (is eq (aref p 0) (aref p 1))
    (is eq (aref p 0) (gethash '?x terms))))


;;; Pattern iterators...

;; `(defun <ws> foo <ws> (x <ws> y) <nl> <ws> (+ <ws> x <ws> y))
;; where <ws> stands for whitespaces and <nl> for newlines

;; To match it against

;; `(defun ?name ?ordinary-lambda-list ?body)

;; I _cannot_ iterate over both in at the same speed.

(define-test+run pattern-iterator
  (false (collect (make-pattern-iterator #())))
  (is equalp '(a) (collect (make-pattern-iterator #(a)))))


;;; bindings

(define-test+run make-binding
  ;; TODO export 'binding, maybe
  (is eq
      (find-class 'binding)
      (class-of (make-binding :?x "a"))
      "Make binding should return an instance of the class \"binding\".")
  (is equal "#<BINDING :?X → A>"
      (prin1-to-string
       (make-binding :?x 'a))
      "The specialized print-object method on the class \"binding\" should print the slots \"from\" and \"to\"."))

(defun bindings-alist (binding-set)
  (etypecase binding-set
      ((eql t) t)
      (null nil)
      (binding
       (list (cons (from binding-set) (to binding-set))))
      (binding-set
       (sort
        (mapcar (lambda (binding)
                  (cons (from binding) (to binding)))
                (alexandria:hash-table-values (bindings binding-set)))
        #'string<
        :key (lambda (binding-cons)
               (symbol-name (car binding-cons)))))))

(define-test+run binding-set
  (is eq
      (find-class 'binding-set)
      (class-of (make-binding-set))
      "make-binding-set should return an instance of the class \"binding-set\".")
  (true (emptyp (make-binding-set))
        "make-binding-set should return an empty set of bindings.")
  (is equal
      "#<BINDING-SET (empty)>"
      (prin1-to-string (make-binding-set))
      "The print-object method specialized on the class \"binding-set\" should print it when the binding set is empty.")
  (is equal
      "#<BINDING-SET ((:?X . #<BINDING :?X → A>))>"
      (prin1-to-string (merge-bindings (make-binding-set)
                                       (make-binding :?x 'a)))
      "The print-object method specialized on the class \"binding-set\" should print the binding when there's only 1.")
  (is equal
      "#<BINDING-SET (2 bindings)>"
      (prin1-to-string
       (merge-bindings (make-binding-set)
                       (merge-bindings (make-binding :?x 'a)
                                       (make-binding :?y 'b))))
      "The print-object method specialized on the class \"binding-set\" should print the number of bindings when there's more than 1.")
  (false
   (let* ((b1 (make-binding-set))
          (b2 (copy-binding-set b1)))
     (eq (bindings b1) (bindings b2)))
   "Copying a bindind-set should create a different hash-table")
  (is-values (find-binding (make-binding-set) :?x)
    (eq nil)
    (eq nil)
    "Should not find a binding in an empty binding-set")
  (is equal '(:?x a)
      (let* ((bs (make-binding-set)))
        (set-binding bs (make-binding :?x 'a))
        (let ((binding (find-binding bs :?x)))
          (list (from binding) (to binding))))
      "Should find the right binding")
  (let* ((bs (make-binding-set)))
    (is eq bs (add-binding bs (make-binding :?x 'a))
        "Adding first bindings should return the binding-set")
    (is eq bs (add-binding bs (make-binding :?y 'b))
        "Adding unrelated bindings should return the binding-set"))
  (let* ((bs (make-binding-set)))
    (add-binding bs (make-binding :?x 'a))
    (is eq bs (add-binding bs (make-binding :?x 'a))
        "Adding the same binding twice should return the binding-set"))
  (let* ((bs (make-binding-set)))
    (add-binding bs (make-binding :?x 'a))
    (false (add-binding bs (make-binding :?x 'b))
           "Adding conflicting bindings should return nil"))
  (is equal '((:?x . a))
      (let* ((bs (make-binding-set)))
        (add-binding bs (make-binding :?x 'a))
        (add-binding bs (make-binding :?x 'b))
        (bindings-alist bs))
      "Adding conflicting bindings should not modify the binding-set"))


(define-test+run merge-bindings
  ;; merging nil and t
  (progn
    (false (merge-bindings nil nil))
    (false (merge-bindings nil t))
    (false (merge-bindings t nil))
    (true (merge-bindings t t)))
  ;; merging with nil
  (progn
    (false (merge-bindings (make-binding :?x 'a) nil)
           "merging with nil should fail")
    (false (merge-bindings nil (make-binding :?x 'a))
           "merging with nil should fail"))
  ;; merging with t
  (progn
    (true (merge-bindings (make-binding :?x 'a) t))
    (true (merge-bindings t (make-binding :?x 'a)))
    (is equal '((:?x . a))
        (bindings-alist (merge-bindings (make-binding :?x 'a) t)))
    (is equal '((:?x . a))
        (bindings-alist (merge-bindings t (make-binding :?x 'a)))))
  (is equal '((:?x . a) (:?y . b))
      (bindings-alist (merge-bindings (make-binding :?x 'a) (make-binding :?y 'b))))
  (finish
   (let ((term (term 'a))
         (bs (make-binding-set)))
     (add-binding bs (make-binding term 42))
     (is eq bs (merge-bindings bs (make-binding term 42)))))
  (finish
   (let ((term (term 'a))
         (bs (make-binding-set)))
     (add-binding bs (make-binding term 42))
     (is eq bs (merge-bindings bs (make-binding term 42))))))

(defun test-match (pattern input)
  (match (compile-pattern pattern) input))

(define-test+run "match basic patterns"
  (true (match nil nil))
  (false (match nil t))
  (false (match t nil))
  (true (match t t))
  (true (match 1 1))
  (false (match 1 2))
  (true (match 'x 'x))
  (true (match "x" "x"))
  (false (match 'x 'y)))

(define-test+run "match wildcard (any)"
  (true (match (any) nil))
  (true (match (any) t))
  (true (match (any) 1))
  (true (match (any) 2))
  (true (match (any) 'x))
  (true (match (any) "x"))
  (true (match (any) 'y)))

;;; TODO check the actual return values
(define-test+run "match terms"
  (true (match (term :?x) nil))
  (true (match (term :?x) 1))
  (true (match (term :?x) 'x))
  (true (match (term :?x) "x"))
  (true (match (term :?x) '(a)))
  (true (match (term :?x) (term :?x)))
  (true (match `#(,(term :?x)) (list 42))))


;;; Sequences

(define-test+run "match sequences"
  (true (match #(a) '(a)))
  (false (match #(a b) #(a)))
  (true (match #(a b) #(a b)))
  (finish
   (multiple-value-bind (bindings iterator)
       (match #(a b) #(a b a))
     (is eq t bindings)
     (false (donep iterator)))))


;;; test :maybe :zero-or-more and :alternation

(defun test-match* (description pattern input expected-binding)
  (finish
   (let* ((pattern (compile-pattern pattern))
         (bindings (match pattern input)))
     (cond
       ((eq expected-binding t)
        (is eq t bindings
            "~a: matching the pattern ~s against the input ~s should have created the bindings ~s but we got ~s instead."
            description pattern input expected-binding bindings))
       (expected-binding
        (and (true bindings
                   "~a: matching the pattern ~s against the input ~s should been successful."
                   description pattern input)
             (of-type 'binding bindings
                      "~a: matching the pattern ~s against the input ~s should return an object of type \"bindings\", got ~s (of type ~s) instead."
                      description pattern input bindings (type-of bindings))
             (is eq pattern (from bindings)
                 "~a: the bindings from matching the pattern ~s against the input ~s should bind ~s, but got ~s instead"
                 description pattern input pattern (from bindings))
             (is equalp expected-binding (to bindings)
                 "~a: the bindings from matching the pattern ~s against the input ~s should bind _to_ ~s, but got ~s instead"
                 description pattern input expected-binding (to bindings))))
       (t
        (false bindings
               "~a: matching the pattern ~s against the input ~s should not have matched"
               description pattern input)))
     bindings)))

(define-test+run "match maybe"
  (test-match* "matching a? against the sequence (a)"
              (maybe 'a) '(a) t)
  (test-match* "matching a? against the atom 'a"
              (maybe 'a) 'a nil)
  (test-match* "matching a? against the empty sequence"
              (maybe 'a) nil t)
  (test-match* "matching a? against the atom 'b"
              (maybe 'a) 'b nil)
  (test-match* "matching (maybe ?x) against the atom 'a"
              (maybe (term '?x)) 'a nil)
  (test-match* "matching (maybe ?x) against the empty sequence"
              (maybe (term '?x)) nil t))

(define-test+run "match alternations"
  (test-match* "matching (or a b) against 'a"
               (alternation #(a b)) 'a 'a)
  (test-match* "matching (or a b) against 'b"
               (alternation #(a b)) 'b 'b)
  (test-match* "matching (or a b) against 'c"
               (alternation #(a b)) 'c nil)
  (test-match* "matching (or ?x b) against 'c"
               (alternation #(a b)) 'c nil)
  (test-match* "matching (or (maybe a) b) against the atom 'a"
               '(:alternation (:maybe a) b) 'a nil)
  (test-match* "matching (or (maybe a) b) against the sequence (a)"
               '(:alternation (:maybe a) b) '(a) '(a))
  (test-match* "matching (or (maybe a) b) against the atom 'b"
               '(:alternation (:maybe a) b) 'b 'b)
  (test-match* "matching (or (maybe a) b) against the sequence (b)"
               '(:alternation (:maybe a) b) '(b) '(b))
  #++ ;; TODO non-greedy repetition
  (let ((binding (test-match pat 'b)))
    (true binding)
    (is eq t binding))
  #++ ;; TODO non-greedy repetition
  (false (test-match pat 'c)))

(define-test+run "match zero-or-more"
  (true (test-match '(:zero-or-more a) nil))
  (true (test-match '(:zero-or-more a b) '(a))
        "It should match 0 times")
  (is eq t (test-match '(:zero-or-more a b) '(a b))
      "It should match 1 time")
  (is eq t (test-match '(:zero-or-more a b) '(a b a))
      "It should match 1 time")
  (is eq t (test-match '(:zero-or-more a b) '(a b a b))
      "It should match twice")
  (false (test-match '(:zero-or-more a b) 'a)))

#++ ;; TODO
(progn
  ;; I want this to be true
  (test-match '(a (:zero-or-more a b)) '(a a b))
  ;; Not this
  (test-match '(a (:zero-or-more a b)) '(a (a b)))
  ;; That one should be used instead of ^^^
  (test-match '(a ((:zero-or-more a b))) '(a (a b))))


;;; Match substitution

(defun test-pattern-substitute (pattern bindings)
  (multiple-value-bind (compiled-pattern term-pool)
      (breeze.pattern:compile-pattern pattern)
    (values (pattern-substitute compiled-pattern bindings) term-pool)))

(defun make-binding-set* (bindings)
  (loop
    :with binding-set = (make-binding-set)
    :for (from . to) :in bindings
    :for binding = (make-binding from to)
    :do (unless (add-binding binding-set binding)
          (error "Failed to add bindings ~s" binding))
    :finally (return binding-set)))

;; (make-binding-set* `((:?x . 24)))

(define-test+run pattern-substitute
  (progn
    (is eq nil (test-pattern-substitute nil nil))
    (is eq t (test-pattern-substitute t nil))
    (is eq 'x (test-pattern-substitute 'x nil)))
  (progn
    (is eq nil (test-pattern-substitute nil t))
    (is eq t (test-pattern-substitute t t))
    (is eq 'x (test-pattern-substitute 'x t)))
  (progn
    (multiple-value-bind (substituted-pattern term-pool)
        (test-pattern-substitute :?x (make-binding-set* '((:?x . 42))))
      (is eq (gethash :?x term-pool) substituted-pattern))
    (is eq t (test-pattern-substitute t t))
    (is eq 'x (test-pattern-substitute 'x t))))


;;; Rules and rewrites


#++
(let ((r (make-rewrite '(/ ?x ?x) 1)))
  (list (pattern= (rewrite-pattern r) #(/ (term :?x) (term :?x)))
        (rewrite-template r)))

#++
(make-rewrite '(/ (* ?x ?y) ?z)
              '(* ?x (/ ?y ?z)))

#++
(make-rewrite '(/ ?x 1) ?x)
