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
Code: (with-buffer (x)
                   (breeze:split-sexp
                         x)

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
Code: (breeze:fill-paragraph x)

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


Name: delete-char should let you delete a character to fix syntax errors
Skip: t ; It's not implemented yet
Code: (breeze:delete-char x)

=-=
(if ¦; test)
=-=
(if ¦ test)
=-=-=

Name: delete-forward-char should let you delete a character to fix syntax errors
Skip: t ; It's not implemented yet
Code: (breeze:delete-forward-char x)

=-=
(if ;¦ test)
=-=
(if ¦ test)
=-=-=
