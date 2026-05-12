(defpackage #:breeze.test.completion
  (:documentation "Tests for the package breeze.completion.")
  (:use #:cl #:breeze.completion #:breeze.parser)
  (:import-from #:breeze.test-helpers
                #:iterator-at)
  (:import-from #:breeze.completion
                #:maybe-previous-node)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.completion)

;;;; WIP

#++
(defun test-maybe-previous-node (string &key expected at)
  (let* (($node (make-node-iterator string)))
    (maybe-previous-node)))

"#|  "

"(make-h-t"

"(make-hash-table :test"
