#|

This file is used to load breeze's system.

It is used, for example, by emacs in breeze.el.

|#

(cl:in-package #:cl-user)

(asdf:load-asd
 (merge-pathnames "../breeze.asd" *load-truename*))

(unless (asdf:component-loaded-p "breeze")
  #+quicklisp
  (ql:quickload "breeze")
  #-quicklisp
  (asdf:load-system '#:breeze)
  (format t "~&Breeze loaded!~%"))
