(defpackage #:breeze.comparison-checks
  (:documentation "Linter checks for misuse of comparison operators.")
  (:use #:cl
        #:breeze.analysis
        #:breeze.checks
        #:breeze.diagnostics))

(in-package #:breeze.comparison-checks)

;; TODO don't trigger this check if there isn't exactly 2 arguments to
;; eq.
(defcheck error-eq-with-literal-number (node-iterator)
  :node-type parens
  "Flag `eq' called with a literal number."
  (unless (quotedp node-iterator)
    (with-match (node-iterator (eq ?a ?b))
      (when (or (and ?a (number-node-p ?a))
                (and ?b (number-node-p ?b)))
        (simple-node-error
         node-iterator
         "Don't use `eq' with literal numbers, it is not portable. Use `eql' instead."
         :replacement "eql")))))
