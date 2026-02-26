
(uiop:define-package #:breeze
    (:documentation "The breeze package is meant for the end-user.")
  (:use #:cl)
  (:use-reexport
   #:breeze.configuration
   #:breeze.command
   #:breeze.command-utils))

(in-package #:breeze)
