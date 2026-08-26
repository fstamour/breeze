(defpackage #:breeze.test.parse-tree
  (:documentation "Test package for #:breeze.parse-tree")
  (:use #:cl #:breeze.parser)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:true
                #:false))

(in-package #:breeze.test.parse-tree)

(define-test+run "diagnostics normalization"
  (isnt eqv
        (string-node 0 42 :errors '(("err")))
        (string-node 0 42)
        "a node without errors should not be equivalent to a node with errors.")
  (isnt eqv
        (string-node 0 42)
        (string-node 0 42 :errors '(("err")))
        "a node with errors should not be equivalent to a node without errors.")
  (is eqv
      (string-node 0 42 :errors '(("err")))
      (string-node 0 42 :errors "err")
      "A list of errors that is a string (not an actual list) should be
normalized to a list of list containing that string.")
  (is eqv
      (string-node 0 42 :errors '(("err")))
      (string-node 0 42 :errors '("err"))
      "A list of errors containing a string should be normalized to a list of
list containing that string.")
  (is eqv
      (string-node 0 42 :errors '(("err")))
      (string-node 0 42 :errors '(nil "err"))
      "A NIL in a list of errors should be ignored (removed)."))
