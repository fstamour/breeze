;;; -*- mode: erts; -*-

Point-Char: $

;; with paredit, you can't use paredit-splice-sexp-killing-backward
;; (M-up) without losing the comment
Name: splice backward keeps comments
Command: breeze:splice-sexp-backwards
;; TODO automatically skip if the "command" doesn't exist
Skip: t

=-=
(progn
  ;; some comment
  $(+ 2 2))
=-=
;; some comment
$(+ 2 2)
=-=-=

;; 2024-12-20 I just found out about emacs' parse-sexp-ignore-comments
;; variable, but its behaviour (when using paredit) seems pretty
;; chaotic (from my 2 min tests).


Name: contextual split sexp
Command: breeze:split-sexp

=-=
(:export
 #:a
 #:b$
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
(if $; test)
=-=
(if $ test)
=-=-=

Name: delete-forward-char should let you delete a character to fix syntax errors
Command: breeze:delete-forward-char

=-=
(if ;$ test)
=-=
(if $ test)
=-=-=


Name: forward-slurp-sexp should not ignore comments
Command: breeze:forward-slurp-sexp

=-=
($) ;; asdf
=-=
($ ;; asdf
 )
=-=-=

=-=
($) #| asdf |#
=-=
($ #| asdf |#)
=-=-=

=-=
($)
#| asdf |#
=-=
($
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
 a$
 ;; 2
 b)
=-=
(;; 1
 a)
;; 2
b
=-=-=


;; With emacs, you can't use kill-sexp (C-M-k) to kill a comment
Name: kill-sexp-comments

=-=
($;; 1
 a)
=-=
($a)
=-=-=

=-=
($;; 1
 ;; 2
 ;; 3
 a)
=-=
($a)
=-=-=

=-=
($
#|
some block comment
|#
 a)
=-=
($a)
=-=-=

;; With emacs, kill-sexp (C-M-k) doesn't parse some things correctly
Name: kill-sexp-multiple-tokens-without-spaces

=-=
$abc,def
=-=
$,def
=-=-=
;; emacs would delete the whole abc,def


;; Like paredit-kill, which is like kill-line, but keeping the
;; structure valid
Name: kill

=-=
(a b)$ ; some comment
=-=
(a b)$
=-=-=

=-=
($a b) ; some comment
=-=
($) ; some comment
=-=-=

=-=
$(a b) ; some comment
=-=
$
=-=-=

=-=
(a "b $c d")
=-=
(a "b $")
=-=-=


;; paredit-backslash is annoying
Name: backslash

;; It should _not_ read a character if just inserting the backslash
;; would resuld in valid code.
=-=
"$n"
=-=
"\n"
=-=-=

;; When deleting an escaped character, it should not delete the
;; backslash along with it, it should intead try to read a new
;; character to escape... probably

Code: breeze-backward-delete

=-=
\n$
=-=
\$
=-=-=

;; It would be nice if "backslash" would insert the char-name of the
;; character when it make sense

Code: \ SPC

=-=
$
=-=
\Space
=-=-=

;; paredit seems to have a silly bug, if you type \, it will ask for a
;; character to escape and if you type \, it will ask for a character
;; to escape and if you type \, it will ask for a character to escape
;; and if you type \, it will ask for a character to escape and if you
;; type \, it will ask for a character to escape and if you type \,

Name: doublequote

;; I don't like that paredit does "forward-char" when I try to enter a
;; double quote at the end of a string. Which happens all the time
;; when I write docstrings.

Code: breeze-doublequote

=-=
"$"
=-=
"\""
=-=-=

;; Paredit doesn't shadow the C-t binding, which is normally bound to transpose-chars.
;; which means that it lets you do this (|) -> )|(
Name: transpose-chars

;; here's a test to make sure it doesn't do that
=-=
(|)
=-=
(|)
=-=
