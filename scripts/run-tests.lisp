(require 'asdf)

;; Load quicklisp _if_ available
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (warn "~%~%======~%Quicklisp is not installed.~%======~%~%")))

;; load local projects first
(let ((local-projects-searcher
        (find-if (lambda (x)
                   (string-equal "local-projects-searcher" (symbol-name x)))
                 asdf:*system-definition-search-functions*)))
  (when local-projects-searcher
    (push local-projects-searcher asdf:*system-definition-search-functions*)))

(asdf:load-asd (truename "breeze.asd"))

#+quicklisp
(ql:quickload "breeze/test" :verbose t)

#-quicklisp
(asdf:load-system "breeze/test")

(breeze.test.main:run-all-tests :exitp t)
