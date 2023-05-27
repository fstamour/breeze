(cl:in-package #:cl-user)

(defpackage #:breeze.test.main
  (:documentation "Entry point for all tests in breeze.")
  (:use #:cl)
  (:export #:run-breeze-tests))

(in-package #:breeze.test.main)

(defun run-breeze-tests (&optional exitp)
  "Run breeze's tests."
  (let ((packages (breeze.xref:find-packages-by-prefix "breeze.test")))
    (format *trace-output*
            "~&About to run tests for the packages: ~{~A~^, ~}"
            packages)
    (finish-output *trace-output*)
    (let ((report (parachute:test packages)))
      (if exitp
          (uiop:quit (if (eq :failed (parachute:status report)) 1 0))
          report))))
