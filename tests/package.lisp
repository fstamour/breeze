
(defpackage #:breeze.test.package
  (:documentation "Tests for the package breeze.package")
  (:use #:cl #:breeze.package #:breeze.analysis)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.package)

;; todo add to asd


#++ ;; Sanity-check
(mapcar #'read-from-string
        '("in-package"
          "common-lisp:in-package"
          "cl:in-package"
          "cl-user::in-package"
          "common-lisp-user::in-package"))

(defun test-in-package-node-p (string)
  ;; The funky reader macro and quasiquote is to fuck with slime and
  ;; sly's regex-based search for "(in-package". Without this the
  ;; rest of the file is evaluated in cl-user by slime and sly.
  (let ((package-designator-node
          #.`(,'in-package-node-p (make-node-iterator string))))
    (when package-designator-node
      (node-string package-designator-node))))

(define-test+run in-package-node-p
  (is equal "x" (test-in-package-node-p "(in-package x)"))
  (is equal nil (test-in-package-node-p "(in-package #)"))
  (is equal ":x" (test-in-package-node-p "(in-package :x)"))
  (is equal "#:x" (test-in-package-node-p "(in-package #:x)"))
  (is equal "\"x\"" (test-in-package-node-p "(in-package \"x\")"))
  (is equal "x" (test-in-package-node-p "( in-package x )"))
  (is equal "x" (test-in-package-node-p "( in-package #| ∿ |# x )"))
  (is equal "x" (test-in-package-node-p "(cl:in-package x)"))
  (is equal "x" (test-in-package-node-p "(cl::in-package x)"))
  (is equal "42" (test-in-package-node-p "(cl::in-package 42)"))
  ;; TODO ? Not sure it's worth it lol...
  ;; (is equal "x" (test-in-package-node-p "('|CL|::|IN-PACKAGE| x)"))
  (is eq nil (test-in-package-node-p "(cl:)"))
  (is eq nil (test-in-package-node-p "'(in-package x)")))
