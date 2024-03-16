
(uiop:define-package #:breeze
    (:documentation "The breeze package meant for the end-user.")
  (:use #:cl)
  (:use-reexport)
  (:import-from #:breeze.command
                #:define-command)
  (:export #:define-command))

(in-package #:breeze)

#++
(let ((package #:breeze.command)
      (symbols '(#:define-command
                 #:read-string-then-insert)))
  (loop import then export))
