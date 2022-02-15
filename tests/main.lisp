(cl:in-package #:cl-user)

(defpackage #:breeze.test.main
  (:use #:cl)
  (:import-from #:breeze.user
                #:current-packages)
  (:import-from #:breeze.test
                #:run-all-tests)
  (:import-from #:breeze.xref
                #:package-test)
  (:documentation "Entry point for all tests in breeze."))

(in-package #:breeze.test.main)

(defun run-breeze-tests ()
  "Load and run breeze's selftests."
  (terpri)
  (loop
    :for package :in (current-packages "^breeze\\.[^.]+.test$")
    :do
       (format t "~&Testing package \"~(~A~)\"." (package-name package))
       (run-all-tests (package-test package))))
