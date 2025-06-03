(defpackage #:breeze.test.pattern
  (:documentation "Test package for breeze.pattern.")
  (:use #:cl #:breeze.pattern #:breeze.iterator)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:true
                #:false
                #:of-type
                #:fail)
  (:import-from #:breeze.pattern
                ;; Structures
                #:ref
                #:ref-name
                #:refp
                #:ref=
                #:term
                #:termp
                #:term-name
                #:term=
                #:typed-term
                #:typed-term-p
                #:typed-term-name
                #:typed-term-type
                #:typed-term=
                #:repetition
                #:repetitionp
                #:repetition=
                #:repetition-pattern
                #:repetition-min
                #:repetition-max
                #:maybe
                #:zero-or-more
                #:alternation
                #:alternationp
                #:alternation-pattern
                #:alternation=
                #:pattern=
                ;; Compilation
                #:term-symbol-p
                #:compile-pattern
                #:ref-pattern
                #:defpattern
                ;; Iterator
                #:pattern-iterator
                #:make-pattern-iterator
                ;; Match
                #:make-binding
                #:merge-bindings
                #:match))

(in-package #:breeze.test.pattern)

(define-test+run ref
  (let ((ref (ref :x)))
    (of-type ref ref)
    (true (refp ref))
    (is eq :x (ref-name ref))))

(define-test+run ref=
  (true (ref= (ref :x) (ref :x)))
  (false (ref= (ref :y) (ref :x)))
  (false (ref= (ref :y) 42)))

(define-test+run term
  (let ((term (term :x)))
    (of-type term term)
    (true (termp term))
    (is eq :x (term-name term))))

(define-test+run term=
  (true (term= (term :x) (term :x)))
  (false (term= (term :y) (term :x)))
  (false (term= (term :y) 42)))

(define-test+run typed-term
  (let ((typed-term (typed-term 'symbol :x)))
    (of-type typed-term typed-term)
    (true (typed-term-p typed-term))
    (is eq :x (typed-term-name typed-term))
    (is eq 'symbol (typed-term-type typed-term))))

(define-test+run typed-term=
  (is typed-term= (term :x) (term :x))
  (isnt typed-term= (term :x) (term :y))
  (let ((expected (typed-term 'symbol :x)))
    (is typed-term= expected (typed-term 'symbol :x))
    (isnt typed-term= expected (typed-term 'symbol :y))
    (isnt typed-term= expected (typed-term 'keyword :x))
    (isnt typed-term= expected (typed-term 'keyword :y))))

(define-test+run maybe
  (let ((maybe (maybe :x)))
    (of-type repetition maybe)
    ;; TODO check repetition-{min,max}
    (is eq :x (repetition-pattern maybe))))

(define-test+run zero-or-more
  (let ((zero-or-more (zero-or-more :x)))
    (of-type repetition zero-or-more)
    ;; TODO check repetition-{min,max}
    (is eq :x (repetition-pattern zero-or-more))))

(define-test+run alternation
  (let ((alternation (alternation :x)))
    (of-type alternation alternation)
    (true (alternationp alternation))
    (is eq :x (alternation-pattern alternation))))

;; TODO alternation=

(define-test+run pattern=
  (is pattern= 'x 'x)
  (is pattern= '(x) '(x))
  (is pattern= (ref 'x) (ref 'x))
  (is pattern= (term 'x) (term 'x))
  (is pattern= (typed-term 'symbol 'x) (typed-term 'symbol 'x))
  (is pattern= (maybe 'y) (maybe 'y))
  (is pattern= (maybe '(x y)) (maybe '(x y)))
  ;; TODO Maybe I should try to detect this case when compiling...
  (is pattern= (maybe (maybe 'x)) (maybe (maybe 'x)))
  (is pattern= (zero-or-more 'y) (zero-or-more 'y))
  (is pattern= (zero-or-more '(x y)) (zero-or-more '(x y)))
  (is pattern= (zero-or-more (maybe 'x)) (zero-or-more (maybe 'x)))
  (is pattern= (alternation 'y) (alternation 'y))
  (is pattern= (alternation '(x y)) (alternation '(x y)))
  (is pattern= (alternation (maybe 'x)) (alternation (maybe 'x))))



(define-test+run term-symbol-p
  (true (term-symbol-p :?x))
  (false (term-symbol-p 'x))
  (false (term-symbol-p "?x")))

#++
(define-test+run ref-symbol-p
  (true (ref-symbol-p :$x))
  (false (ref-symbol-p 'x))
  (false (ref-symbol-p "$x")))

(define-test+run compile-pattern
  (is pattern= :x (compile-pattern :x))
  (is pattern= 42 (compile-pattern 42))
  (is pattern= (term :?x) (compile-pattern :?x))
  (is pattern= (ref :x) (compile-pattern '(:ref :x)))
  (is pattern= (maybe :x) (compile-pattern '(:maybe :x)))
  (is pattern= (maybe :x :?y) (compile-pattern '(:maybe :x :?y)))
  (is pattern= (maybe #(:x :y)) (compile-pattern '(:maybe (:x :y))))
  (is pattern= (zero-or-more #(:x)) (compile-pattern '(:zero-or-more :x)))
  (is pattern= (zero-or-more #(:x :y)) (compile-pattern '(:zero-or-more :x :y)))
  (is pattern= (alternation #(:x)) (compile-pattern '(:alternation :x)))
  (is pattern= (alternation #(:x :y)) (compile-pattern '(:alternation :x :y)))
  (multiple-value-bind (p terms)
      (compile-pattern '(?x ?x))
    (is eq (aref p 0) (aref p 1))
    (is eq (aref p 0) (gethash '?x terms))))



(defpattern a
    a ?a)

(defpattern b
  (:ref a) (:ref a))


;;; Pattern iterators...

;; `(defun <ws> foo <ws> (x <ws> y) <nl> <ws> (+ <ws> x <ws> y))
;; where <ws> stands for whitespaces and <nl> for newlines

;; To match it against

;; `(defun ?name ?ordinary-lambda-list ?body)

;; I _cannot_ iterate over both in at the same speed.

(define-test+run pattern-iterator
  (false (collect (make-pattern-iterator #())))
  (is equalp '(a) (collect (make-pattern-iterator #(a))))
  (is equalp
      '(a #s(term :name ?a))
      ;; == (coerce (ref-pattern 'a) 'list)
      (collect (make-pattern-iterator `#(,(ref 'a)))))
  (is equalp
      '(a #s(term :name ?a) a #s(term :name ?a))
      (collect (make-pattern-iterator `#(,(ref 'b)))))
  (is equalp
      `(b ,@(coerce (ref-pattern 'a) 'list) c)
      (collect (make-pattern-iterator `#(b ,(ref 'a) c)))))



(define-test+run merge-bindings
  (false (merge-bindings nil nil))
  (false (merge-bindings nil t))
  (false (merge-bindings t nil))
  (true (merge-bindings t t))
  (false (merge-bindings (make-binding :?x 'a) nil))
  (false (merge-bindings nil (make-binding :?x 'a)))
  (is equal '((:?x . a)) (merge-bindings (make-binding :?x 'a) t))
  (is equal '((:?x . a)) (merge-bindings t (make-binding :?x 'a)))
  (is equal '((:?x . a) (:?y . b)) (merge-bindings (make-binding :?x 'a) (make-binding :?y 'b)))
  (let ((term (term 'a)))
    (is equal `((,term . 42))
        (merge-bindings `((,term . 42)) `((,term . 42))))))

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

;;; TODO check the actual return values
(define-test+run "match terms"
  (true (match (term :?x) nil))
  (true (match (term :?x) 1))
  (true (match (term :?x) 'x))
  (true (match (term :?x) "x"))
  (true (match (term :?x) '(a)))
  (true (match (term :?x) (term :?x)))
  (true (match `#(,(term :?x)) (list 42))))

;;; TODO check the actual return values
(define-test+run "match typed-terms"
  (true (match (typed-term 'null :?x) nil))
  (false (match (typed-term 'null :?x) t))
  (true (match (typed-term 'number :?x) 1))
  (false (match (typed-term 'number :?x) t))
  (true (match (typed-term 'symbol :?x) 'x))
  (false (match (typed-term 'keyword :?x) 'x))
  (true (match (typed-term 'string :?x) "x"))
  (false (match (typed-term 'string :?x) 1))
  (true (match (typed-term 'cons :?x) '(a)))
  (false (match (typed-term 'cons :?x) 'a)))


;;; Sequences

(define-test+run "match sequences"
  (true (match #(a) '(a)))
  (false (match #(a b) #(a)))
  (true (match #(a b) #(a b)))
  (false (match #(a b) #(a b a))))


;;; test :maybe :zero-or-more and :alternation

#++ ;; TODO
(define-test+run "match maybe"
  (is eq t (match (maybe 'a) 'a))
  (is eq t (match (maybe 'a) nil))
  (is eq t (match (maybe 'a :?x) nil))
  (false (match (maybe 'a :?x) 'b))
  (false (match (maybe 'a) 'b))
  (is equalp `(,(maybe :name :?x :pattern a) a) (match (maybe 'a :?x) 'a))
  (is equalp '(#(term :name '?x) a) (match (maybe (term '?x)) 'a))
  (is equalp `(,(term :name '?x) nil) (match (maybe (term '?x)) nil)))

#++ ;; TODO
(define-test+run "match alternations"
  (is eq t (test-match '(:alternation a b) 'a))
  (is eq t (test-match '(:alternation a b) 'b))
  (false (test-match '(:alternation a b) 'c))
  (is equalp '(#s(term :name ?x) c) (test-match '(:alternation ?x b) 'c))
  (let ((pat (compile-pattern '(:alternation (:maybe a ?x) b))))
    (is equalp `(,(maybe :name ?x :pattern a) a) (test-match pat 'a))
    (is eq t (test-match pat 'b))
    (false (test-match pat 'c))))

#++ ;; TODO
(define-test+run "match zero-or-more"
  (true (test-match '(:zero-or-more a) nil))
  (false (test-match '(:zero-or-more a b) '(a)))
  (is eq t (test-match '(:zero-or-more a b) '(a b)))
  (false (test-match '(:zero-or-more a b) '(a b a)))
  (is eq t (test-match '(:zero-or-more a b) '(a b a b)))
  (false (test-match '(:zero-or-more a b) 'a)))

#++ ;; TODO
(progn
  ;; I want this to be true
  (test-match '(a (:zero-or-more a b)) '(a a b))
  ;; Not this
  (test-match '(a (:zero-or-more a b)) '(a (a b)))
  ;; That one should be used instead of ^^^
  (test-match '(a ((:zero-or-more a b))) '(a (a b))))



;;; Testing patterns with references in them

(defpattern optional-parameters
    &optional
    (:zero-or-more
     (:alternation (:the symbol ?var)
                   ((:the symbol ?var)
                    ?init-form (:maybe (:the symbol ?supplied-p-parameter))))))

#++
(match (ref 'optional-parameters)
  '(&optional))

#++
(match (ref 'optional-parameters)
  '(&optional x))

#++
(list
 '(&optional x)
 '(&optional (x 1))
 '(&optional (x 1 supplied-p))
 '(&optional x y (z t)))


(defpattern rest-parameter &rest ?var)

(defpattern body-parameter &body ?var)

(defpattern key-parameters
    &key
    (:zero-or-more
     (:alternation (:the symbol ?var)
                   ((:the symbol ?var)
                    ?init-form (:maybe (:the symbol ?supplied-p-parameter))))))

(defpattern aux-parameters
    &aux
    (:zero-or-more
     (:alternation (:symbol ?var)
                   ((:symbol ?var) ?init-form)))
  (:maybe &allow-other-keys))

(defpattern ordinary-lambda-list
    (:zero-or-more ?var)
    (:ref optional-parameters)
  (:ref rest-parameter)
  (:ref key-parameters)
  (:ref aux-parameters))

(defpattern defun
    (defun (:the symbol ?name) $ordinary-lambda-list ?body))





(defun test-match-ref (pattern input &key bindings)
  (let ((result (match pattern input)))
    (if bindings
        (is equalp bindings (if (listp result)
                                (mapcar (lambda (x)
                                          (cons (term-name (car x)) (cdr x)))
                                        result)
                                result)
            "Matching the pattern ~s agains the input ~s should have created the bindings ~s but we got ~s instead."
            pattern input bindings result)
        (false result))))


(define-test+run "match ref"
  (test-match-ref (ref 'a) '(a 42) :bindings '((?a . 42)))
  (test-match-ref (ref 'b) '(a 42 a 73))
  ;; TODO What if we want to use the pattern 'a with independent bindings?
  ;; Idea new syntax: (ref 'a ('?a ?a1)) or (ref 'a :prefix a1)
  (test-match-ref (ref 'b) '(a 42 a 42) :bindings '((?a . 42)))
  (test-match-ref (ref 'body-parameter) '(42))
  (test-match-ref (ref 'body-parameter) '(&body 42)
                  :bindings '((?var . 42))))



;;; Match substitution

(defun test-pattern-substitute (pattern bindings)
  (multiple-value-bind (compiled-pattern term-pool)
      (breeze.pattern:compile-pattern pattern)
    (let ((actual-bindings
            (sublis (alexandria:hash-table-alist term-pool) bindings)))
      (pattern-substitute compiled-pattern actual-bindings))))


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
