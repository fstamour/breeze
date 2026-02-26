(defpackage #:breeze.test.cli
  (:documentation "Tests for the package breeze.cli")
  (:use #:cl)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:is-values
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.cli)
