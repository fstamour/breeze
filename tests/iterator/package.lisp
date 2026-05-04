(defpackage #:breeze.test.iterator
  (:documentation "Tests for the package breeze.iterator")
  (:use #:cl #:breeze.iterator)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:true
                #:false
                #:of-type
                #:fail))
