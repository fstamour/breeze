
(ql:quickload "swank")

;; See the content of the variable slime-required-modules in emacs to
;; get the list availabe modules. See the "needed" varible of
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

;; install breeze's dependencies
(asdf:load-asd (truename "breeze.asd"))

(ql:quickload
 (remove-if
  (lambda (system-name)
    (string= "breeze/config" system-name))
  (asdf:system-depends-on (asdf:find-system "breeze"))))

(uiop:dump-image "dependencies.core")
