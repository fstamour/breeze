;;; These are drafts of tests for breeze.el

;;; (ert-test-erts-file ...)

(breeze-eval "(+ 1 2)")
;; ("" "3 (2 bits, #x3, #o3, #b11)")

;; Should error
;; (breeze-eval "")

(breeze-eval "(error \"oups\")")

(and
 (eq t (breeze-eval-predicate "t"))
 (eq t (breeze-eval-predicate "T"))
 (eq nil (breeze-eval-predicate "nil"))
 (eq nil (breeze-eval-predicate "NIL")))


(breeze-interactive-eval "(read)")

(breeze-interactive-eval "(error \"oupsie\")")



(breeze-check-if-connected-to-listener)

(breeze-ensure-breeze t)

(breeze-init)



(breeze-validate-if-package-exists "CL")
;; t

(breeze-validate-if-package-exists "this package probably doesn't exists")
;; nil

(breeze-validate-if-breeze-package-exists)



(breeze-system-definition)
