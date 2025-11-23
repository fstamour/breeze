(in-package #:breeze.test.pattern)



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

(define-test+run multi-valued-var-symbol-p
  (true (multi-valued-var-symbol-p :?*x))
  (false (multi-valued-var-symbol-p :?x))
  (false (multi-valued-var-symbol-p 'x))
  (false (multi-valued-var-symbol-p "?x")))

(define-test+run wildcard-symbol-p
  (true (wildcard-symbol-p :_x))
  (false (wildcard-symbol-p 'x))
  (false (wildcard-symbol-p "_x")))



(define-test+run "compile-pattern - wildcard"
  (is eqv (wildcard) (compile-pattern :_x)))

(define-test+run "compile-pattern - atoms"
  (is eqv :x (compile-pattern :x))
  (is eqv (svar :?x) (compile-pattern :?x))
  (is eqv (svar '?x) (compile-pattern '?x))
  (is eqv
      (svar '?*x :multi-valued-p t)
      (compile-pattern '?*x))
  (is eqv 42 (compile-pattern 42))
  (is eqv "abc" (compile-pattern "abc")))

(define-test+run "compile-pattern - :var"
  (is eqv (var :?y (wildcard)) (compile-pattern '(:var :?y _)))
  (is eqv (var :?z (svar '?a)) (compile-pattern '(:var :?z ?a)))
  (let ((p (compile-pattern '(?x ?x))))
    (is eq (name (aref p 0)) (name (aref p 1)))))

(define-test+run "compile-pattern - :maybe"
  (is eqv (maybe :x) (compile-pattern '(:maybe :x)))
  (is eqv (maybe #(:x :y)) (compile-pattern '(:maybe (:x :y))))
  (is eqv (maybe :x :?y) (compile-pattern '(:maybe :x :?y)))
  (is eqv (maybe :x :?z) (compile-pattern '(:named :?z (:maybe :x))))
  (is eqv (maybe (svar :?x)) (compile-pattern '(:maybe :?x)))
  (is eqv (maybe (var :?x (wildcard))) (compile-pattern '(:maybe (:var :?x :_)))))

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
