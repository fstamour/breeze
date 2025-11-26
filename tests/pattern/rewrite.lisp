(in-package #:breeze.test.pattern)

;;; Match substitution

(defun test-pattern-substitute (pattern bindings)
  (let ((compiled-pattern (compile-pattern pattern)))
    (pattern-substitute compiled-pattern bindings)))

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
                :?x (substitutions '((:?x 42)))))
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
