(in-package #:breeze.test.pattern)

(defun alist->substitutions (bindings)
  "Helper to create a `substitution' object from an alist."
  (loop
    :with substitutions = (make-substitutions)
    :for (from . to) :in bindings
    :for binding = (make-binding from to)
    :do (unless (add-binding substitutions binding)
          (error "Failed to add bindings ~s" binding))
    :finally (return substitutions)))

;; (alist->substitutions `((:?x . 24)))


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

(define-test+run "match - simple-var"
  (is eqv (make-binding :?x nil) (match (svar :?x) nil))
  (is eqv (make-binding :?x 1) (match (svar :?x) 1))
  (is eqv (make-binding :?x 'x) (match (svar :?x) 'x))
  (is eqv (make-binding :?x "x") (match (svar :?x) "x"))
  (is eqv (make-binding :?x '(a)) (match (svar :?x) '(a)))
  (is eqv (make-binding :?x (svar :?x)) (match (svar :?x) (svar :?x)))
  (let ((v #(42)))
    (is eqv
        (alist->substitutions `((:?x . ,(make-tree-iterator v))))
        (match (vector (svar :?x)) v))))

(define-test+run "match - var"
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
                  (when bindings
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
                         (test-binding binding)))))
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
              (maybe (svar '?x)) 'a nil)
  (test-match* "matching (maybe ?x) against the empty sequence"
              (maybe (svar '?x)) #() t))

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


;;; Matching "sym"

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
