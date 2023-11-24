;;; -*- mode: erts; -*-

Point-Char: ¦

;; with paredit, you can't use paredit-splice-sexp-killing-backward
;; (M-up) without losing the comment
Name: splice backward keeps comments
Skip: t ; It's not implemented yet

=-=
(progn
  ;; some comment
  ¦(+ 2 2))
=-=
;; some comment
¦(+ 2 2)
=-=-=


Name: contextual split sexp
Skip: t ; It's not implemented yet

=-=
(:export
 #:a
 #:b¦
 #:c
 #:d)
=-=
(:export
 #:a
 #:b
(:export
 #:c
 #:d)
=-=-=
