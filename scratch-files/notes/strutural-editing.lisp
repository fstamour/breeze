;;; -*- mode: erts; -*-

Point-Char: ¦

;; with paredit, you can't use paredit-splice-sexp-killing-backward
;; (M-up) without losing the comment
Name: splice backward keeps comments
Command: breeze:splice-sexp-backwards
;; TODO automatically skip if the "command" doesn't exist
Skip: t

=-=
(progn
  ;; some comment
  ¦(+ 2 2))
=-=
;; some comment
¦(+ 2 2)
=-=-=


Name: contextual split sexp
Command: breeze:split-sexp

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
Command: breeze:fill-paragraph

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


;; TODO:
;; C-<left> and C-<right> don't work inside comments (M-<left> and
;; M-<right> do what I instinctively think C-<left> and C-<right>
;; should do).

;; TODO:
;; M-" doesn't warp the next expression in quotes inside comments.


Name: delete-char should let you delete a character to fix syntax errors
Command: breeze:delete-char

=-=
(if ¦; test)
=-=
(if ¦ test)
=-=-=

Name: delete-forward-char should let you delete a character to fix syntax errors
Command: breeze:delete-forward-char

=-=
(if ;¦ test)
=-=
(if ¦ test)
=-=-=


Name: forward-slurp-sexp should not ignore comments
Command: breeze:forward-slurp-sexp

=-=
(¦) ;; asdf
=-=
(¦ ;; asdf
 )
=-=-=

=-=
(¦) #| asdf |#
=-=
(¦ #| asdf |#)
=-=-=

=-=
(¦)
#| asdf |#
=-=
(¦
 #| asdf |#)
=-=-=

Name: quickfix #:#:

=-=
#:#:x
=-=
#:x
=-=-=


Name: quickfix spaces between closing parens

=-=
(() )
=-=
(())
=-=-=


=-=
((
 ) )
=-=
((
 ))
=-=-=


=-=
((
 )
)
=-=
((
 ))
=-=-=


Name: forward-barf-sexp and comments

=-=
(;; 1
 a¦
 ;; 2
 b)
=-=
(;; 1
 a)
;; 2
b
=-=-=
