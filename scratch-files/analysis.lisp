(in-package #:breeze.test.analysis)

;; TODO move somewhere else
;; maybe add a file "debug utilities"
(defmacro with-trace ((&rest spec) &body body)
  `(progn
     ,@(loop :for s :in spec :when (listp s) :collect `(trace ,@s))
     (trace ,@(loop :for s :in spec :unless (listp s) :collect s))
     (unwind-protect
          (progn ,@body)
       (untrace ,@(loop :for s :in spec
                        :collect (if (listp s) (car s) s))))))

(with-trace ((match :methods t)
             breeze.pattern::go-down-together
             breeze.pattern::go-up-together)
  (match (compile-pattern '(if ?cond)) (parse "(if a)")))

(with-trace (eqv)
  (is eqv
      (substitutions `((?cond ,(iterator-value (token 4 5 :name "A")))))
      (match (compile-pattern '(if ?cond)) (parse "(if a)")
        :skipp #'whitespace-or-comment-node-p)))

(with-trace ((match :methods t)
             next
             breeze.pattern::go-down-together
             breeze.pattern::go-up-together)
  (match (compile-pattern '(if (:maybe ?cond))) (parse "(if)")))

(with-trace (match ;;(match :methods t)
                breeze.pattern::go-down-together
              breeze.pattern::go-up-together)
  (test-malformed-if-node-p "(if a b c d e)"))


(with-trace (match ;;(match :methods t)
                breeze.pattern::go-down-together
              breeze.pattern::go-up-together)
  (test-malformed-if-node-p "(if a b)"))

(with-trace ((match :methods t))
  (match (compile-pattern '(if ?cond)) (parse "(if)")))
