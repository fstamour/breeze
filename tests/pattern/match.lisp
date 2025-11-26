(in-package #:breeze.test.pattern)

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
        (substitutions `((:?x ,(make-tree-iterator v))))
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
            (substitutions `((:?x ,(make-tree-iterator v))))
            ;; the #1# makes the test result more readable
            (match (vector (var :?x 73)) #1#)))
    (let ((v #(42)))
      (is eqv
          (substitutions `((:?x ,(make-tree-iterator v))))
          (match (vector (var :?x 42)) v)))))


;;; Pattern iterators

(define-test+run "pattern iterators"
  (true (donep (make-tree-iterator #())))
  (false (donep (make-tree-iterator #(a)))))


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

;; TODO export and re-use in tests/analysis.lisp
(defun check-substitution (&key description
                             expected-substitution actual-substitution
                           &allow-other-keys)
  (breeze+parachute:pass-p ('check-substitution)
    (is eqv
        expected-substitution
        actual-substitution
        ;; "~a: ~&"
        "~a~&  expected the substitution~%  ~s~&but got ~%  ~s~%instead"
        description
        expected-substitution actual-substitution)))

(defun check-if-iterators-are-updated
    (&key
       description
       actual-substitution
       $pattern-before $pattern-after
       $input-before $input-after
     &allow-other-keys)
  "- pattern is advanced by 1 if there was a match, otherwise it is untouched
     - input is advanced if there was a match, otherwise, it is
       untouched"
  (cond
    ((donep $pattern-before)
     (is eqv $pattern-before $pattern-after
         "~a  the pattern iterator should not be modified when the pattern iterator already done before matching"
         description)
     (is eqv $input-before $input-after
         "~a  the input iterator should not be modified when the pattern iterator is already done before matching"
         description))
    ((null actual-substitution)
     (is eqv $pattern-before $pattern-after
         "~a  the pattern iterator should not be modified when there is no match"
         description)
     (is eqv $input-before $input-after
         "~a  the input iterator should not be modified when there is no match"
         description))
    (actual-substitution
     (isnt eqv $pattern-before $pattern-after)
     (isnt eqv $input-before $input-after))))

(defun check-iterator-depths (&key description
                                $pattern-before $pattern-after
                                $input-before $input-after
                              &allow-other-keys)
  "- the pattern is at the same depth as before
     - the input is at the same depth as before"
  (breeze+parachute:pass-p ('check-iterator-depths)
    (is = (slot-value $pattern-before 'depth)
        (slot-value $pattern-after 'depth)
        "~a  the depth of the pattern iterator should be the same after and before the match"
        description)
    (is = (slot-value $input-before 'depth)
        (slot-value $input-after 'depth)
        "~a  the depth of the input iterator should be the same after and before the match"
        description)))

(defun test-match-iterator
    (description pattern input
     &key skipp
       expected-substitution)
  (breeze+parachute:pass-p ('test-match-iterator)
    (let* ((compiled-pattern (compile-pattern pattern))
           ($pattern (make-pattern-iterator compiled-pattern))
           ($pattern-before (copy-iterator $pattern))
           ($input (make-tree-iterator input))
           ($input-before (copy-iterator $input))
           (substitutions (match $pattern $input :skipp skipp)))
      (let ((kwargs (list
                     :description (format nil "when ~a:~%(matching the pattern ~s against the input ~s)~%"
                                          description pattern input)
                     :pattern pattern
                     :compiled-pattern compiled-pattern
                     :$pattern-before $pattern-before
                     :$pattern-after $pattern
                     :input input
                     :$input-before $input-before
                     :$input-after $input
                     :skipp skipp
                     :actual-substitution substitutions
                     :expected-substitution expected-substitution)))
        (loop :for invariant :in '(check-substitution
                                   check-if-iterators-are-updated
                                   check-iterator-depths)
              :always (apply invariant kwargs)))
      substitutions)))

(define-test+run "match iterators"
  (test-match-iterator
   "matching an empty sequence pattern against an empty input sequence"
   #() #()
   :expected-substitution t)
  (test-match-iterator
   "matching an empty sequence pattern against a non-empty input sequence"
   #() #(a)
   :expected-substitution t)
  (loop :for test-case :in (list
                            #()
                            #(a)
                            #(b c)
                            #(d e f))
        :do
           (test-match-iterator
            "matching a sequence pattern against an identical input sequence"
            test-case test-case
            :expected-substitution t)
           (test-match-iterator
            "matching a sequence pattern of one simple-var against an input sequence"
            '(?x) test-case
            :expected-substitution
            (unless (zerop (length test-case))
              (substitutions `((?x ,(let ((it (make-tree-iterator test-case)))
                                      (go-down it)
                                      it))))))))

#++
(trace match
       :wherein test-match-iterator)

;; (test-match-iterator "asdf" '(?x) #(a))


;;; test :maybe :zero-or-more and :either

(defun test-match* (description pattern input expected-substitution)
  "Compile pattern, match against input then validate that the substitution returned are as expected."
  (finish (let* ((compiled-pattern (compile-pattern pattern))
                 (substitution (match compiled-pattern input)))
            (is eqv expected-substitution substitution
                "~a: ~&matching the pattern ~s~&against the input~&~s~&should return the substitution ~s,~&but got ~s instead"
                description pattern input expected-substitution substitution)
            substitution)
          description))

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
  (test-match* "Matching (:zero-or-more a b) against #(a)"
               '(:zero-or-more a b)
               #(a)
               t)
  (test-match* "Matching (:zero-or-more a b) against #(a b)"
               '(:zero-or-more a b)
               #(a b)
               t)
  (test-match* "Matching (:zero-or-more a b) against #(a b a)"
               '(:zero-or-more a b) #(a b a)
               t)
  (test-match* "Matching (:zero-or-more a b) against #(a b a b)"
               '(:zero-or-more a b) #(a b a b)
               t)
  (false (test-match '(:zero-or-more a b) 'a)
         "It should not match against a symbol (the input must be a vector.")
  (test-match* "Matching (a (:zero-or-more b c) against (a b c)"
               '(a (:zero-or-more b c)) #(a b c)
               t)
  (test-match* "Matching (a (:zero-or-more a b)) against (a (a b))"
               '(a (:zero-or-more a b))
               #(a (a b))
               t)
  (test-match* "Matching (a ((:zero-or-more a b))) against (a (a b))"
               '(a ((:zero-or-more a b)))
               #(a (a b))
               nil))


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
