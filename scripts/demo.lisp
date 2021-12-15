;; Setup a lisp process before making a demo

(cl:in-package #:cl)

(ql:quickload '(swank breeze))

(swank:create-server :port 40050 :dont-close t)
