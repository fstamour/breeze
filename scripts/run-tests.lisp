(require 'asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (warn "~%~%======~%Quicklisp is not installed.~%======~%~%")))

(asdf:load-asd (truename "breeze.asd"))

#+quicklisp
(ql:quickload "breeze/test" :verbose t)

#-quicklisp
(asdf:load-system "breeze/test")

(breeze.test.main:run-all-tests :exitp t)
