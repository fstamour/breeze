;; || represents the cursor

(progn
  ;; some comment
  ||(+ 2 2))

;; with paredit, you can't use paredit-splice-sexp-killing-backward
;; (M-up) without losing the comment
