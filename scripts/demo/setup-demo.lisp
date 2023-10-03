
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

(format t "~&About to load breeze.asd...~%")
(force-output)

;; install breeze's dependencies
(asdf:load-asd (truename "breeze.asd"))

(format t "~&About to quickload breeze's dependencies...~%")
(force-output)

(ql:quickload
 (remove-if
  (lambda (system-name)
    (string= "breeze/config" system-name))
  (asdf:system-depends-on (asdf:find-system "breeze"))))

(format t "~&About to dump core \"dependencies.core\"...~%")
(force-output)

(uiop:dump-image "dependencies.core")
