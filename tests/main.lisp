(cl:in-package #:cl-user)

(defpackage #:breeze.test.main
  (:documentation "Entry point for all tests in breeze.")
  (:use #:cl))

(in-package #:breeze.test.main)

(defun run-breeze-tests ()
  "Run breeze's tests."
  (let ((packages (breeze.xref:find-packages-by-prefix "breeze.test")))
    (format *debug-io*
            "~&About to run tests for the packages: ~{~A~^, ~}"
            packages)
    (finish-output *debug-io*)
    (parachute:test packages)))
