(defpackage #:breeze.setup-demo
  (:documentation "Load and configure everything needed for a demonstration.")
  (:use #:cl))

(in-package #:breeze.setup-demo)

(ql:quickload "swank")

;; See the content of the variable slime-required-modules in emacs to
;; get the list available modules. See the "needed" variable of
;; #'slime-load-contribs to know which ones are actually needed.
;;
;; Those are the ones required (transitively) by the slime-fancy
;; contrib.
(swank:swank-require
 '(#:swank-indentation
   #:swank-trace-dialog
   #:swank-package-fu
   #:swank-presentations
   #:swank-macrostep
   #:swank-fuzzy
   #:swank-fancy-inspector
   #:swank-c-p-c
   #:swank-arglists
   #:swank-repl))
