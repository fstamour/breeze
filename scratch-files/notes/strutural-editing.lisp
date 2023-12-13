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
 #:b)
(:export
 #:c
 #:d)
=-=-=

Name: fill-paragraph in multiline-comments
Skip: t ; It's not implemented yet

=-=
#| this is a long line this is a long line this is a long line this is a long line |#
=-=
#| this is a long line this is a long line this is a long line this is
a long line |#
=-=-=

; The current behaviour of emacs is completely wrong:
; =-=
; #| this is a long line this is a long line this is a long line this is a long line |#
; =-=
; #| this is a long line this is a long line this is a long line this is
; #| a long line |#
; =-=-=

; =-=
; #| this is a long line this is a long line this is a long line this is a long line |#
; #| this is a long line this is a long line this is a long line this is a long line |#
; =-=
; #| this is a long line this is a long line this is a long line this is
; #| a long line |# this is a long line this is a long line this is a
; #| long line this is a long line |#
; =-=-=



;; C-<left> and C-<right> don't work inside comments (M-<left> and
;; M-<right> do what I instinctively think C-<left> and C-<right>
;; should do).

;; M-" doesn't warp the next expression in quotes inside comments.
