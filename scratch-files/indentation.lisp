
;;; Documentations

;; https://joaotavora.github.io/sly/#Semantic-indentation
;; https://slime.common-lisp.dev/doc/html/Semantic-indentation.html#Semantic-indentation
;; Info node `(elisp) Indenting Macros'


;;; Trivial-indent common lisp library

;; trivial-indent "just" configures slynk or swank
(ql:quickload "trivial-indent")

(trivial-indent:define-indentation defmacro (4 &lambda &body))
(trivial-indent:define-indentation something-more-complex (4 &rest (&whole 2 0 4 &body)))

;; what exactly does "trivial-indent:define-indentation" updates?
;; How does it relate to the "slime-cl-indent" contrib?
;;
;; Ah! "swank-indentation" package is defined in `slime/contrib/swank-indentation.lisp'
;; And it is indeed the "back-end" part of the "slime-cl-indent" contrib.
;;
;; it calls `update-indentation-information` in either
;; "swank-indentation" or "slynk/indentation" package.

swank-indentation:update-indentation-information
slynk/indentation:update-indentation-information

;; `trivial-indent:initialize-slime' and
;; `trivial-indent:initialize-sly' sets the variables
;; `swank-indentation:*application-hints-tables*' and
;; `slynk/indentation:*application-hints-tables*'

;; it does keep a hash-table *indentation-hints*


;;; slime-cl-indent elisp slime contrib

;; ‘slime-cl-indent.el’
;; elisp variables
;; - common-lisp-style
;; - common-lisp-styles
;; macro
;; `define-common-lisp-style'

;; slime-cl-indent "just" configures emacs' `lisp-mode'


;;; emacs' built-in `lisp-mode'

;; ‘lisp-mode.el’ defines a bunch of variables to control the indentation:

(
 lisp-align-keywords-in-calls
 lisp-backquote-indentation
 lisp-indent-defun-method
 lisp-indent-maximum-backtracking
 lisp-lambda-list-indentation
 lisp-lambda-list-keyword-alignment
 lisp-lambda-list-keyword-parameter-alignment
 lisp-lambda-list-keyword-parameter-indentation
 lisp-loop-body-forms-indentation
 lisp-loop-clauses-indentation
 lisp-loop-indent-body-forms-relative-to-loop-start
 lisp-loop-indent-forms-like-keywords
 lisp-loop-indent-subclauses
 lisp-simple-loop-indentation
 lisp-tag-body-indentation
 lisp-tag-indentation
)

interactive function: `indent-sexp'


;;; emacs' built-in syntax.el

`parse-partial-sexp'
`syntax-ppss' ;; Parse Partial Sexp State

;; parse-partial-sexp is a primitive-function in ‘src/syntax.c’.



;;; Examples that I want to work better

;; @ is where the cursor/point is at

;; paredit-reindent-defun does nothing is this case, but indent-sexp works
(defun f (x)
x)
