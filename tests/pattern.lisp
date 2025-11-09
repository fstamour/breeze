(defpackage #:breeze.test.pattern
  (:documentation "Test package for breeze.pattern.")
  (:use #:cl #:breeze.pattern #:breeze.iterator)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:is-values
                #:isnt
                #:true
                #:false
                #:of-type
                #:fail
                #:finish)
  ;; importing non-exported symbols
  (:import-from #:breeze.pattern
                #:symbol-starts-with
                #:var-symbol-p
                #:wildcard-symbol-p
                #:bindings
                #:make-substitutions
                #:copy-substitutions
                #:set-binding
                #:add-binding
                #:emptyp))

(in-package #:breeze.test.pattern)

(define-test+run wildcard
  (let ((wildcard (wildcard)))
    (of-type wildcard wildcard)
    (true (wildcardp wildcard)))
  (is eq (wildcard) (wildcard)
      "The fuction (wildcard) always return the same instance."))

(define-test+run var
  (let ((var (var :x)))
    (of-type var var)
    (true (varp var))
    (is eq :x (name var))
    (true (wildcardp (pattern var))))
  (let ((var (var :y 42)))
    (of-type var var)
    (true (varp var))
    (is eq :y (name var))
    (is eql 42 (pattern var)))
  (let ((var (var :z nil)))
    (of-type var var)
    (true (varp var))
    (is eq :z (name var))
    (null (pattern var))))

(define-test+run "var - eqv"
  (true (eqv (var :x) (var :x)))
  (true (eqv (var :x nil) (var :x nil)))
  (true (eqv (var :x 31) (var :x 31)))
  (false (eqv (var :x) (var :x 31)))
  (false (eqv (var :y) (var :x)))
  (false (eqv (var :y) 42)))

(define-test+run "var - print-object"
  (is string= "(var :x)" (prin1-to-string (var :x)))
  (is string= "(var :y nil)" (prin1-to-string (var :y nil)))
  ;; TODO would be nice if it printed as "(var :z 'a)"
  (is string= "(var :z a)" (prin1-to-string (var :z 'a)))
  (is string= "(var :za 32)" (prin1-to-string (var :za 32))))

(define-test+run "sym - print-object"
  (is string= "(sym 'cl :wild)"
      (format nil "~a" (sym 'cl :wild)))
  (is string= "(sym 'cl 'defun)"
      (format nil "~a" (sym 'cl 'defun)))
  (is string= "(sym 'cl '#:defun :possibly-internal-symbol)"
      (format nil "~a" (sym 'cl '#:defun :possibly-internal-symbol)))
  (is string=
      "(sym (find-package '#:UIOP/PACKAGE) \"DEFINE-PACKAGE\")"
      (let ((symbol 'uiop:define-package))
        (princ-to-string (sym (symbol-package symbol)
                              (symbol-name symbol))))))

(define-test+run "sym - :wild symbol"
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
  (is eq t (match (sym :keyword :wild) keyword:defun))
  ;; match against '|defun|
  (is eq t (match (sym :wild :wild) '|defun|))
  (is eq t (match (sym #.*package* :wild) '|defun|))
  (is eq nil (match (sym '#:cl :wild) '|defun|))
  (is eq t (match (sym #.*package* :wild) '|defun|)))

(define-test+run "sym - :wild package"
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

(define-test+run "sym - with both non :wild package and symbol"
  ;; current package
  (is eq t (match (sym #.*package* 'defun) 'defun))
  (is eq nil (match (sym #.*package* 'defun) '|defun|))
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
  (is eq nil (match (sym nil '#:defun) '#:|defun|))
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

(define-test+run eqv
  (is eqv 'x 'x)
  (is eqv '(x) '(x))
  (is eqv (var 'x) (var 'x))
  (is eqv (maybe 'y) (maybe 'y))
  (is eqv (maybe '(x y)) (maybe '(x y)))
  ;; TODO Maybe I should try to detect this case when compiling...
  (is eqv (maybe (maybe 'x)) (maybe (maybe 'x)))
  (is eqv (zero-or-more 'y) (zero-or-more 'y))
  (is eqv (zero-or-more '(x y)) (zero-or-more '(x y)))
  (is eqv (zero-or-more (maybe 'x)) (zero-or-more (maybe 'x)))
  (is eqv (either #(y)) (either #(y)))
  (is eqv (either #(x y)) (either #(x y)))
  (is eqv
      (either (vector (maybe 'x)))
      (either (vector (maybe 'x)))))



(define-test+run symbol-starts-with
  (progn
    (false (symbol-starts-with 'x #\?))
    (true (symbol-starts-with :? #\?))
    (true (symbol-starts-with :?x #\?))
    (true (symbol-starts-with '? #\?))
    (true (symbol-starts-with '?x #\?)))
  (progn
    (false (symbol-starts-with 'x "?"))
    (true (symbol-starts-with :? "?"))
    (true (symbol-starts-with :?x "?"))
    (true (symbol-starts-with '? "?"))
    (true (symbol-starts-with '?x "?"))))

(define-test+run var-symbol-p
  (true (var-symbol-p :?x))
  (false (var-symbol-p 'x))
  (false (var-symbol-p "?x")))

;; TODO multi-valued-var-symbol-p

(define-test+run wildcard-symbol-p
  (true (wildcard-symbol-p :_x))
  (false (wildcard-symbol-p 'x))
  (false (wildcard-symbol-p "_x")))

(define-test+run "compile-pattern - wildcard"
  (is eqv (wildcard) (compile-pattern :_x)))

(define-test+run "compile-pattern - atoms"
  (is eqv :x (compile-pattern :x))
  (is eqv 42 (compile-pattern 42))
  (is eqv "abc" (compile-pattern "abc")))

(define-test+run "compile-pattern - :var"
  (is eqv (var :?x) (compile-pattern :?x))
  (is eqv (var :?y) (compile-pattern '(:var :?y _)))
  (is eqv (var :?z (var '?a)) (compile-pattern '(:var :?z ?a)))
  (let ((p (compile-pattern '(?x ?x))))
    (is eq (name (aref p 0)) (name (aref p 1)))))

(define-test+run "compile-pattern - :maybe"
 (is eqv (maybe :x) (compile-pattern '(:maybe :x)))
 (is eqv (maybe #(:x :y)) (compile-pattern '(:maybe (:x :y)))))

(define-test+run "compile-pattern - :zero-or-more"
   (is eqv (zero-or-more #(:x)) (compile-pattern '(:zero-or-more :x)))
   (is eqv (zero-or-more #(:x :y)) (compile-pattern '(:zero-or-more :x :y))))

(define-test+run "compile-pattern - :either"
   (is eqv (either #(:x)) (compile-pattern '(:either :x)))
   (is eqv (either #(:x :y)) (compile-pattern '(:either :x :y))))

(define-test+run "compile-pattern - :symbol"
  (is eqv (sym :wild :wild :wild) (compile-pattern '(:symbol)))
  (is eqv (sym :wild :defun :wild) (compile-pattern '(:symbol :defun)))
  (is eqv (sym :cl :defun :wild) (compile-pattern '(:symbol :defun :cl)))
  (is eqv (sym :cl :defun :qualified) (compile-pattern '(:symbol :defun :cl :qualified))))


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
  (is equal "#<binding :?x → a>"
      (prin1-to-string
       (make-binding :?x 'a))
      "The specialized print-object method on the class \"binding\" should print the slots \"from\" and \"to\"."))

(defun bindings-alist (substitutions)
  (etypecase substitutions
      ((eql t) t)
      (null nil)
      (binding
       (list (cons (from substitutions) (to substitutions))))
      (substitutions
       (sort
        (mapcar (lambda (binding)
                  (cons (from binding) (to binding)))
                (alexandria:hash-table-values (bindings substitutions)))
        #'string<
        :key (lambda (binding-cons)
               (symbol-name (car binding-cons)))))))

(define-test+run substitutions
  (of-type substitutions
      (make-substitutions)
      "make-substitutions should return an instance of the class \"substitutions\".")
  (true (emptyp (make-substitutions))
        "make-substitutions should return an empty set of bindings.")
  (is equal
      "#<substitutions (empty)>"
      (prin1-to-string (make-substitutions))
      "The print-object method specialized on the class \"substitutions\" should print it when the binding set is empty.")
  (is equal
      "#<substitutions ((:?x . #<binding :?x → a>))>"
      (prin1-to-string (merge-substitutions (make-substitutions)
                                       (make-binding :?x 'a)))
      "The print-object method specialized on the class \"substitutions\" should print the binding when there's only 1.")
  (is equal
      "#<substitutions (2 bindings)>"
      (prin1-to-string
       (merge-substitutions (make-substitutions)
                       (merge-substitutions (make-binding :?x 'a)
                                       (make-binding :?y 'b))))
      "The print-object method specialized on the class \"substitutions\" should print the number of bindings when there's more than 1.")
  (false
   (let* ((b1 (make-substitutions))
          (b2 (copy-substitutions b1)))
     (eq (bindings b1) (bindings b2)))
   "Copying a bindind-set should create a different hash-table")
  (is-values (find-binding (make-substitutions) :?x)
    (eq nil)
    (eq nil)
    "Should not find a binding in an empty substitutions")
  (is equal '(:?x a)
      (let* ((bs (make-substitutions)))
        (set-binding bs (make-binding :?x 'a))
        (let ((binding (find-binding bs :?x)))
          (list (from binding) (to binding))))
      "Should find the right binding")
  (let* ((bs (make-substitutions)))
    (is eq bs (add-binding bs (make-binding :?x 'a))
        "Adding first bindings should return the substitutions")
    (is eq bs (add-binding bs (make-binding :?y 'b))
        "Adding unrelated bindings should return the substitutions"))
  (let* ((bs (make-substitutions)))
    (add-binding bs (make-binding :?x 'a))
    (is eq bs (add-binding bs (make-binding :?x 'a))
        "Adding the same binding twice should return the substitutions"))
  (let* ((bs (make-substitutions)))
    (add-binding bs (make-binding :?x 'a))
    (false (add-binding bs (make-binding :?x 'b))
           "Adding conflicting bindings should return nil"))
  (is equal '((:?x . a))
      (let* ((bs (make-substitutions)))
        (add-binding bs (make-binding :?x 'a))
        (add-binding bs (make-binding :?x 'b))
        (bindings-alist bs))
      "Adding conflicting bindings should not modify the substitutions"))


(define-test+run merge-substitutions
  ;; merging nil and t
  (progn
    (false (merge-substitutions nil nil))
    (false (merge-substitutions nil t))
    (false (merge-substitutions t nil))
    (true (merge-substitutions t t)))
  ;; merging with nil
  (progn
    (false (merge-substitutions (make-binding :?x 'a) nil)
           "merging with nil should fail")
    (false (merge-substitutions nil (make-binding :?x 'a))
           "merging with nil should fail"))
  ;; merging with t
  (progn
    (true (merge-substitutions (make-binding :?x 'a) t))
    (true (merge-substitutions t (make-binding :?x 'a)))
    (is equal '((:?x . a))
        (bindings-alist (merge-substitutions (make-binding :?x 'a) t)))
    (is equal '((:?x . a))
        (bindings-alist (merge-substitutions t (make-binding :?x 'a)))))
  (is equal '((:?x . a) (:?y . b))
      (bindings-alist (merge-substitutions (make-binding :?x 'a) (make-binding :?y 'b))))
  (finish
   (let ((var (var 'a))
         (bs (make-substitutions)))
     (add-binding bs (make-binding var 42))
     (is eq bs (merge-substitutions bs (make-binding var 42)))))
  (finish
   (let ((var (var 'a))
         (bs (make-substitutions)))
     (add-binding bs (make-binding var 42))
     (is eq bs (merge-substitutions bs (make-binding var 42))))))

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
(define-test+run "match vars"
  ;; no sub-patterns
  (progn
    (is eqv (make-binding :?x nil) (match (var :?x) nil))
    (is eqv (make-binding :?x 1) (match (var :?x) 1))
    (is eqv (make-binding :?x 'x) (match (var :?x) 'x))
    (is eqv (make-binding :?x "x") (match (var :?x) "x"))
    (is eqv (make-binding :?x '(a)) (match (var :?x) '(a)))
    (is eqv (make-binding :?x (var :?x)) (match (var :?x) (var :?x)))
    (let ((v #(42)))
      (is eqv
          (alist->substitutions `((:?x . ,(make-tree-iterator v))))
          (match (vector (var :?x)) v))))
  ;; with sub-pattenr
  (progn
    (false (match (var :?x t) nil))
    (is eqv (make-binding :?x nil) (match (var :?x nil) nil))
    (false (match (var :?x 2) 1))
    (is eqv (make-binding :?x 1) (match (var :?x 1) 1))
    (false (match (var :?x 'y) 'x))
    (is eqv (make-binding :?x 'x) (match (var :?x 'x) 'x))
    (false (match (var :?x "y") "x"))
    (is eqv (make-binding :?x "x") (match (var :?x "x") "x"))
    (false (match (var :?x '(b)) '(a)))
    (is eqv (make-binding :?x '(a)) (match (var :?x '(a)) '(a)))
    (false (match (var :?x 'y) (var :?x 'x)))
    (is eqv (make-binding :?x (var :?x 'x)) (match (var :?x 'x) (var :?x 'x)))
    (let ((v #1=#(42)))
      (isnt eqv
            (alist->substitutions `((:?x . ,(make-tree-iterator v))))
            ;; the #1# makes the test result more readable
            (match (vector (var :?x 73)) #1#)))
    (let ((v #(42)))
      (is eqv
          (alist->substitutions `((:?x . ,(make-tree-iterator v))))
          (match (vector (var :?x 42)) v)))))


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
                  (isnt eq t bindings
                        "~a: ~&matching the pattern~&~s~&against the input~&~s~&should not return T as bindings"
                        description pattern input)
                  (etypecase bindings
                    ;; binding
                    (binding (test-binding bindings))
                    ;; substitutions
                    (substitutions
                     (let ((binding (find-binding bindings pattern)))
                       (true bindings
                             "~a: ~&matching the pattern ~s against the input ~s return a `substitutions', but no binding for the pattenr was found."
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
              (maybe (var '?x)) 'a nil)
  (test-match* "matching (maybe ?x) against the empty sequence"
              (maybe (var '?x)) #() t))

(define-test+run "match eithers"
  (test-match* "matching (or a b) against 'a"
               (either #(a b)) 'a t)
  (test-match* "matching (or a b) against 'b"
               (either #(a b)) 'b t)
  (test-match* "matching (or a b) against 'c"
               (either #(a b)) 'c nil)
  (test-match* "matching (or ?x b) against 'c"
               (either #(a b)) 'c nil)
  (test-match* "matching (or (maybe a) b) against the atom 'a"
               '(:either (:maybe a) b) 'a nil)
  (test-match* "matching (or (maybe a) b) against the sequence (a)"
               '(:either (:maybe a) b) '(a) nil)
  (test-match* "matching (or (maybe a) b) against the atom 'b"
               '(:either (:maybe a) b) 'b t)
  (test-match* "matching (or (maybe a) b) against the sequence (b)"
               '(:either (:maybe a) b) '(b) nil)
  (is eq 'a (value (to (find-binding
                        (match (compile-pattern '(:either (x ?x) (y ?y)))
                          #(x a))
                        '?x))))
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
               "TODO: need to implement `breeze.generics:eqv' for substitutions (and update `test-match*').")
  #++
  (test-match* "Matching (a (* a b)) against (a (a b))"
               '(a (:zero-or-more a b))
               #(a (a b))
               "TODO: need to implement `breeze.generics:eqv' for substitutions (and update `test-match*')."
               #++
               `(:bindings (t)
                 :$start :_
                 :$end :_
                 :times 1))
  #++
  (test-match* "Matching (a ((*a b))) against (a (a b))"
               '(a ((:zero-or-more a b)))
               #(a (a b))
               "TODO: need to implement `breeze.generics:eqv' for substitutions (and update `test-match*')."))

;;; Match substitution

(defun test-pattern-substitute (pattern bindings)
  (let ((compiled-pattern (compile-pattern pattern)))
    (pattern-substitute compiled-pattern bindings)))

(defun alist->substitutions (bindings)
  (loop
    :with substitutions = (make-substitutions)
    :for (from . to) :in bindings
    :for binding = (make-binding from to)
    :do (unless (add-binding substitutions binding)
          (error "Failed to add bindings ~s" binding))
    :finally (return substitutions)))

;; (alist->substitutions `((:?x . 24)))

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
    (is eql 42 (test-pattern-substitute
                :?x (alist->substitutions '((:?x . 42)))))
    (is eq t (test-pattern-substitute t t))
    (is eq 'x (test-pattern-substitute 'x t))))


;;; Rules and rewrites


#++
(let ((r (make-rewrite '(/ ?x ?x) 1)))
  (list (eqv (rewrite-pattern r) #(/ (var :?x) (var :?x)))
        (rewrite-template r)))

#++
(make-rewrite '(/ (* ?x ?y) ?z)
              '(* ?x (/ ?y ?z)))

#++
(make-rewrite '(/ ?x 1) ?x)

;; TODO this could be a command:
;; (parachute:test *package*)



;;; Parsing cl:loop using patterns

#++ ;; sample "loop" forms
(let ((samples `((loop for el in list do (print el))
                 (loop :repeat 10 #:collect 'bob)
                 (loop :for (head . tail) :on lst :while tail append tail)
                 (loop :for c across string thereis (char= c #\+))
                 (loop :for (k v) :on plist :by #'cddr
                       :sum v :into vsum
                       :max k :into kmax
                       :finally (return (values kmax vsum)))
                 (loop)))))


;; TODO it might be easier to debug if each patterns are compiled separatedly
#++
(let* ((name-clause '((:symbol named) ?name))
       ;; TODO not supported: improper list (the "cons")
       (d-type-spec (let* ((cons (cons nil nil))
                           (sub-patterns (vector cons
                                                 (var '?type-specifier)))
                           (pattern (either sub-patterns)))
                      (setf (car cons) pattern
                            (cdr cons) pattern)
                      pattern))
       (destructured-type-spec `((:symbol of-type) d-type-spec))
       (simple-type-spec '(:either fixnum float t nil))
       (type-spec `(:either ,simple-type-spec ,destructured-type-spec))
       (with-clause `((:symbol with)
                      ?*var1
                      ;; TODO (:maybe typespec)
                      ;; this is not currently possible because the matching is too greedy and it's not possible
                      ;; the "typespec" should not match a "=" or "and"
                      (:maybe ((:symbol =) ?*form1))
                      ;; TODO this kind of repetitions could be
                      ;; handled by a special pattern, not sure about
                      ;; the name "joined-repetition"? it's a
                      ;; repitition with a separation (here it's 'and)
                      (:zero-or-more (:symbol and)
                                     ?*var2
                                     ?*form2
                                     ;; (:maybe typespec)
                                     )))
       ;; a compound-form is a non-empty list [which is a form; a
       ;; special form; a lambda form; a macro form; or a function
       ;; form.
       ;; TODO I could use a "one-or-more" here
       (compound-form (vector (repetition (wildcard) 1)))
       (compound-form+ (repetition compound-form 1))
       (initial-final `((:either (:symbol initially)
                                 (:symbol finally))
                        ,compound-form+))
       (for-as-clause :todo)
       (unconditional `(:either
                        ((:either (:symbol do) (:symbol doing)) ,compound-form+)
                        ((:symbol return)
                         (:either (:symbol it) ?return-form))))
       (list-accumulation `((:either (:symbol collect)
                                     (:symbol collecting)
                                     (:symbol append)
                                     (:symbol appending)
                                     (:symbol nconc)
                                     (:symbol nconcing))
                            (:either (:symbol it) ?form)
                            (:maybe (:symbol into) ,simple-var)))
       (numeric-accumulation `())
       (accumulation `(:either list-accumulation numeric-accumulation))
       (main-clause `(:either
                      unconditional
                      accumulation
                      conditional
                      termination-test
                      ,initial-final))
       (variable-clause `(:either ,with-clause ,initial-final ,for-as-clause))
       (loop-grammar `(loop (:maybe ,@name-clause)
                            (:zero-or-more ,@variable-clause)
                            (:zero-or-more ,@main-clause))))
  ())


#++
(match (compile-pattern loop-grammar)
  #(loop for))
