(defpackage #:breeze.test.logging
  (:documentation "Tests for breeze.logging.")
  (:use #:cl)
  (:import-from #:breeze.logging
                #:compare-level)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false))

(in-package #:breeze.test.logging)

(define-test+run compare-level
  (false (compare-level #'< :debug :debug))
  (false (compare-level #'< :info :debug))
  (true (compare-level #'< :debug :critical))
  (false (compare-level #'< :critical :debug)))
