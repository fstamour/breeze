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
                #:wildcard-symbol-p
                #:bindings
                #:make-binding-set
                #:copy-binding-set
                #:set-binding
                #:add-binding
                #:emptyp))

(in-package #:breeze.test.pattern)

(define-test+run wildcard
  (let ((wildcard (wildcard)))
    (of-type wildcard wildcard)
    (true (wildcardp wildcard))))

(define-test+run term
  (let ((term (term :x)))
    (of-type term term)
    (true (termp term))
    (is eq :x (name term))))

(define-test+run term=
  (true (term= (term :x) (term :x)))
  (false (term= (term :y) (term :x)))
  (false (term= (term :y) 42)))

(define-test+run "sym print-object"
  ;; TODO would be nice if it printed :wild instead of ':wild
  (is string= "(sym 'cl ':wild)"
      (format nil "~a" (sym 'cl :wild)))
  (is string="(sym 'cl 'defun)"
      (format nil "~a" (sym 'cl 'defun)))
  ;; TODO sym with qualification
  ;; TODO uninterned symbols for name or package-name
  )

(define-test+run "sym :wild symbol"
  ;; Match against 'cl:defun
  (is eq t (match (sym :wild :wild) 'defun))
  (is eq t (match (sym "COMMON-LISP" :wild) 'defun))
  (is eq t (match (sym #.*package* :wild) 'defun))
  (is eq t (match (sym :cl :wild) 'defun))
  (is eq t (match (sym 'cl :wild) 'defun))
  (is eq t (match (sym '#:cl :wild) 'defun))
  (is eq t (match (sym 'cl-user :wild) 'defun))
  (is eq t (match (sym "CL" :wild) 'defun))
  (is eq nil (match (sym nil :wild) 'defun))
  ;; match against '#:defun
  (is eq t (match (sym :wild :wild) '#:defun))
  (is eq nil (match (sym '#:cl :wild) '#:defun))
  (is eq nil (match (sym #.*package* :wild) '#:defun))
  (is eq nil (match (sym "CL" :wild) '#:defun))
  (is eq t (match (sym nil :wild) '#:defun))
  ;; match against :defun
  (is eq t (match (sym :wild :wild) :defun))
  (is eq nil (match (sym '#:cl :wild) :defun))
  (is eq nil (match (sym #.*package* :wild) :defun))
  (is eq t (match (sym :keyword :wild) :defun))
  (is eq t (match (sym "KEYWORD" :wild) :defun))
  ;; match against '|defun|
  (is eq t (match (sym :wild :wild) '|defun|))
  (is eq t (match (sym #.*package* :wild) '|defun|))
  (is eq nil (match (sym '#:cl :wild) '|defun|))
  (is eq t (match (sym #.*package* :wild) '|defun|)))

(define-test+run "sym :wild package"
  ;; name = nil
  (is eq nil (match (sym :wild nil) 'defun))
  (is eq t (match (sym :wild nil) nil))
  (is eq t (match (sym :wild nil) :nil))
  ;; name = t
  (is eq nil (match (sym :wild t) nil))
  (is eq t (match (sym :wild t) t))
  (is eq t (match (sym :wild t) :t))
  ;; match against 'cl:defun
  (is eq t (match (sym :wild "DEFUN") 'defun))
  (is eq t (match (sym :wild :defun) 'defun))
  (is eq t (match (sym :wild '#:defun) 'defun))
  (is eq t (match (sym :wild 'defun) 'defun))
  (is eq nil (match (sym :wild "defun") 'defun))
  (is eq nil (match (sym :wild '|defun|) 'defun))
  (is eq nil (match (sym :wild :|defun|) 'defun))
  (is eq nil (match (sym :wild '#:|defun|) 'defun))
  ;; match against '#:defun
  (is eq t (match (sym :wild "DEFUN") '#:defun))
  (is eq t (match (sym :wild :defun) '#:defun))
  (is eq t (match (sym :wild '#:defun) '#:defun))
  (is eq t (match (sym :wild 'defun) '#:defun))
  (is eq nil (match (sym :wild "defun") '#:defun))
  (is eq nil (match (sym :wild '|defun|) '#:defun))
  (is eq nil (match (sym :wild :|defun|) '#:defun))
  (is eq nil (match (sym :wild '#:|defun|) '#:defun))
  ;; match against :defun
  (is eq t (match (sym :wild "DEFUN") :defun))
  (is eq t (match (sym :wild :defun) :defun))
  (is eq t (match (sym :wild '#:defun) :defun))
  (is eq t (match (sym :wild 'defun) :defun))
  (is eq nil (match (sym :wild "defun") :defun))
  (is eq nil (match (sym :wild '|defun|) :defun))
  (is eq nil (match (sym :wild :|defun|) :defun))
  (is eq nil (match (sym :wild '#:|defun|) :defun))
  ;; match against '|defun|
  (is eq nil (match (sym :wild "DEFUN") '|defun|))
  (is eq nil (match (sym :wild :defun) '|defun|))
  (is eq nil (match (sym :wild '#:defun) '|defun|))
  (is eq nil (match (sym :wild 'defun) '|defun|))
  (is eq t (match (sym :wild "defun") '|defun|))
  (is eq t (match (sym :wild '|defun|) '|defun|))
  (is eq t (match (sym :wild :|defun|) '|defun|))
  (is eq t (match (sym :wild '#:|defun|) '|defun|))
  ;; match against :|defun|
  (is eq nil (match (sym :wild "DEFUN") :|defun|))
  (is eq nil (match (sym :wild :defun) :|defun|))
  (is eq nil (match (sym :wild '#:defun) :|defun|))
  (is eq nil (match (sym :wild 'defun) :|defun|))
  (is eq t (match (sym :wild "defun") :|defun|))
  (is eq t (match (sym :wild '|defun|) :|defun|))
  (is eq t (match (sym :wild :|defun|) :|defun|))
  ;; match against '#:|defun|
  (is eq nil (match (sym :wild "DEFUN") '#:|defun|))
  (is eq nil (match (sym :wild :defun) '#:|defun|))
  (is eq nil (match (sym :wild '#:defun) '#:|defun|))
  (is eq nil (match (sym :wild 'defun) '#:|defun|))
  (is eq t (match (sym :wild "defun") '#:|defun|))
  (is eq t (match (sym :wild '|defun|) '#:|defun|))
  (is eq t (match (sym :wild :|defun|) '#:|defun|))
  (is eq t (match (sym :wild '#:|defun|) '#:|defun|)))

(define-test+run "sym with both non :wild package and symbol"
  ;; current package
  (is eq t (match (sym #.*package* 'defun) 'defun))
  (is eq nil (match (sym #.*package* 'defun) '|defun|)) ;; <=
  (is eq t (match (sym #.*package* "defun") '|defun|))
  (is eq nil (match (sym #.*package* 'defun) :|defun|))
  (is eq nil (match (sym #.*package* "defun") :|defun|))
  (is eq nil (match (sym #.*package* 'defun) '#:|defun|))
  (is eq nil (match (sym #.*package* "defun") '#:|defun|))
  ;; cl package
  (is eq t (match (sym '#:cl '#:defun) 'defun))
  (is eq nil (match (sym '#:cl '#:defun) '|defun|))
  ;; nil package (uninterned)
  (is eq nil (match (sym nil '#:defun) '|defun|))
  (is eq nil (match (sym nil '#:defun) '#:|defun|)) ;; <=
  (is eq nil (match (sym nil '#:defun) :defun))
  ;; keyword package
  (is eq t (match (sym :keyword '#:defun) :defun))
  (is eq t (match (sym :keyword :defun) :defun))
  (is eq nil (match (sym :keyword :defun) 'defun)))

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

(define-test+run either
  (let ((either (either #(:x))))
    (of-type either either)
    (true (eitherp either))
    (is equalp #(:x) (patterns either))))

;; TODO either=

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
  (is pattern= (either #(y)) (either #(y)))
  (is pattern= (either #(x y)) (either #(x y)))
  (is pattern=
      (either (vector (maybe 'x)))
      (either (vector (maybe 'x)))))



(define-test+run term-symbol-p
  (true (term-symbol-p :?x))
  (false (term-symbol-p 'x))
  (false (term-symbol-p "?x")))

(define-test+run wildcard-symbol-p
  (true (wildcard-symbol-p :_x))
  (false (wildcard-symbol-p 'x))
  (false (wildcard-symbol-p "_x")))

(define-test+run compile-pattern
  (is pattern= (wildcard) (compile-pattern :_x))
  (is pattern= :x (compile-pattern :x))
  (is pattern= 42 (compile-pattern 42))
  (is pattern= (term :?x) (compile-pattern :?x))
  (is pattern= (maybe :x) (compile-pattern '(:maybe :x)))
  (is pattern= (maybe #(:x :y)) (compile-pattern '(:maybe (:x :y))))
  (is pattern= (zero-or-more #(:x)) (compile-pattern '(:zero-or-more :x)))
  (is pattern= (zero-or-more #(:x :y)) (compile-pattern '(:zero-or-more :x :y)))
  (is pattern= (either #(:x)) (compile-pattern '(:either :x)))
  (is pattern= (either #(:x :y)) (compile-pattern '(:either :x :y)))
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
  (of-type binding
           (make-binding :?x "a")
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
  (of-type binding-set
      (make-binding-set)
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
  "Compile pattern then match against input."
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

(define-test+run "match wildcard"
  (true (match (wildcard) nil))
  (true (match (wildcard) t))
  (true (match (wildcard) 1))
  (true (match (wildcard) 2))
  (true (match (wildcard) 'x))
  (true (match (wildcard) "x"))
  (true (match (wildcard) 'y)))

;;; TODO check the actual return values
(define-test+run "match terms"
  (true (match (term :?x) nil))
  (true (match (term :?x) 1))
  (true (match (term :?x) 'x))
  (true (match (term :?x) "x"))
  (true (match (term :?x) '(a)))
  (true (match (term :?x) (term :?x)))
  (true (match (vector (term :?x)) #(42))))


;;; Sequences

(define-test+run "match sequences"
  (false (match #(a) '(a)))
  (false (match #(a b) #(a)))
  (true (match #(a b) #(a b)))
  (finish
   (multiple-value-bind (bindings iterator)
       (match #(a b) #(a b a))
     (is eq t bindings)
     (false (donep iterator)))))


;;; test :maybe :zero-or-more and :either

(defun test-match* (description pattern input expected-binding)
  "Compile pattern, match against input then validate that the bindings returned are as expected."
  (flet ((do-test-match* ()
           (let* ((pattern (compile-pattern pattern))
                  (bindings (match pattern input)))
             (flet ((stop ()
                      ;; TODO need to see if this makes the failing
                      ;; tests easier to understabd or not.
                      ;; (return-from do-test-match* bindings)
                      )
                    (test-binding (binding)
                      (is eq pattern (from binding)
                          "~a: ~&the bindings from matching the pattern ~s~&against the input~&~s~&should bind ~s,~&but got ~s instead"
                          description pattern input pattern (from binding))))
               ;; === bindings not null ===
               (when expected-binding
                 (true bindings
                       "~a: ~&matching the pattern ~s against the input ~s should been successful."
                       description pattern input)
                 (unless bindings (stop)))
               (cond
                 ;; === T ===
                 ((eq expected-binding t)
                  (is eq t bindings
                      "~a: ~&matching the pattern~&~s~&against the input~&~s~&should have created the bindings~&~s but we got~&~s instead."
                      description pattern input expected-binding bindings))
                 ;; === expected binding ===
                 (expected-binding
                  (etypecase bindings
                    ;; binding
                    (binding (test-binding bindings))
                    ;; binding-set
                    (binding-set
                     (let ((binding (find-binding bindings pattern)))
                       (true bindings
                             "~a: ~&matching the pattern ~s against the input ~s return a `binding-set', but no binding for the pattenr was found."
                             description pattern input)
                       (unless binding (stop))
                       (test-binding binding))))
                  (is eqv expected-binding (to bindings)
                      "~a: ~&the bindings from matching the pattern~&~s against the input~&~s~&should bind _to_~&~s,~&but got ~s instead"
                      description pattern input expected-binding (to bindings)))
                 ;; === (null expected-binding) ===
                 ((null expected-binding)
                  (false bindings
                         "~a: ~&matching the pattern~&~s~&against the input ~s should not have matched"
                         description pattern input))))
             bindings)))
    (finish (do-test-match*) description)))

(define-test+run "match maybe"
  (test-match* "matching a? against the sequence (a)"
              (maybe 'a) '(a) nil)
  (test-match* "matching a? against the atom 'a"
              (maybe 'a) 'a nil)
  (test-match* "matching a? against the empty sequence"
              (maybe 'a) #() t)
  (test-match* "matching a? against the atom 'b"
              (maybe 'a) 'b nil)
  (test-match* "matching (maybe ?x) against the atom 'a"
              (maybe (term '?x)) 'a nil)
  (test-match* "matching (maybe ?x) against the empty sequence"
              (maybe (term '?x)) #() t))

(define-test+run "match eithers"
  (test-match* "matching (or a b) against 'a"
               (either #(a b)) 'a 'a)
  (test-match* "matching (or a b) against 'b"
               (either #(a b)) 'b 'b)
  (test-match* "matching (or a b) against 'c"
               (either #(a b)) 'c nil)
  (test-match* "matching (or ?x b) against 'c"
               (either #(a b)) 'c nil)
  (test-match* "matching (or (maybe a) b) against the atom 'a"
               '(:either (:maybe a) b) 'a nil)
  (test-match* "matching (or (maybe a) b) against the sequence (a)"
               '(:either (:maybe a) b) '(a) nil)
  (test-match* "matching (or (maybe a) b) against the atom 'b"
               '(:either (:maybe a) b) 'b 'b)
  (test-match* "matching (or (maybe a) b) against the sequence (b)"
               '(:either (:maybe a) b) '(b) nil)
  #++ ;; TODO non-greedy repetition
  (let ((binding (test-match pat 'b)))
    (true binding)
    (is eq t binding))
  #++ ;; TODO non-greedy repetition
  (false (test-match pat 'c)))

;; TODO test (:zero-or-more :wildcard)

;; TODO actually check the content of :$start and :$end
(define-test+run "match zero-or-more"
  (is eq t (test-match '(:zero-or-more a) #()))
  (test-match* "Matching (* a b) against #(a)"
               '(:zero-or-more a b)
               #(a)
               `(:bindings ()
                 :$start :_
                 :$end :_
                 :times 0))
  (test-match* "Matching (* a b) against #(a b)"
               '(:zero-or-more a b)
               #(a b)
               `(:bindings (t)
                 :$start :_
                 :$end :_
                 :times 1))
  (test-match* "Matching (* a b) against #(a b a)"
               '(:zero-or-more a b) #(a b a)
               `(:bindings (t)
                 :$start :_
                 :$end :_
                 :times 1))
  (test-match* "Matching (* a b) against #(a b a b)"
               '(:zero-or-more a b) #(a b a b)
               `(:bindings (t t)
                 :$start :_
                 :$end :_
                 :times 2))
  (false (test-match '(:zero-or-more a b) 'a)
         "It should not match against a symbol (the input must be a vector.")
  #++
  (test-match* "Matching (a (* b c) against (a b c)"
               '(a (:zero-or-more b c)) #(a b c)
               "TODO: need to implement `breeze.generics:eqv' for binding-set (and update `test-match*').")
  #++
  (test-match* "Matching (a (* a b)) against (a (a b))"
               '(a (:zero-or-more a b))
               #(a (a b))
               "TODO: need to implement `breeze.generics:eqv' for binding-set (and update `test-match*')."
               #++
               `(:bindings (t)
                 :$start :_
                 :$end :_
                 :times 1))
  #++
  (test-match* "Matching (a ((*a b))) against (a (a b))"
               '(a ((:zero-or-more a b)))
               #(a (a b))
               "TODO: need to implement `breeze.generics:eqv' for binding-set (and update `test-match*')."))

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

;; TODO this could be a command:
;; (parachute:test *package*)
