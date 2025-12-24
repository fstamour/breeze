(defpackage #:breeze.cli
  (:documentation "")
  (:use #:cl)
  (:export #:main))

(in-package #:breeze.cli)

(require 'asdf)

(defun main ()
  (format t "args: ~s~&*features*: ~s~&packages: ~s~&"
          (uiop:command-line-arguments)
          *features*
          (list-all-packages)))


#|

org-tag-alist

orgtbl-mode

| he  |   |
|-----+---|
| loo |   |
| dfa |   |
|     |   |

|#
