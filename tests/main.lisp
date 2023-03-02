(cl:in-package #:cl-user)

(defpackage #:breeze.test.main
  (:documentation "Entry point for all tests in breeze.")
  (:use #:cl)
  (:export #:run-breeze-tests))

(in-package #:breeze.test.main)

(defun run-breeze-tests (&optional exitp)
  "Run breeze's tests."
  (let ((packages (breeze.xref:find-packages-by-prefix "breeze.test")))
    (format *debug-io*
            "~&About to run tests for the packages: ~{~A~^, ~}"
            packages)
    (finish-output *debug-io*)
    (if exitp
        (parachute:test-toplevel packages)
        (parachute:test packages))))
