
(uiop:define-package #:breeze.user
    (:use :cl)
  (:documentation "This package is meant to be used by the end user.")
  (:nicknames :br :br-user :breeze)
  (:shadowing-import-from #:breeze.definition
                          #:defun
                          #:fmakunbound)
  (:import-from #:breeze.documentation
		#:find-undocumented-symbols)
  (:import-from #:breeze.definition
                #:*function-redifinition-hooks*
                #:function-body)
  (:import-from #:breeze.test
                #:*test-change-hooks*
                #:deftest
                #:test-body
                #:test
                #:run-test
                #:run-all-tests
                #:is)
  (:import-from #:breeze.test-runner
                #:start-test-runner
                #:stop-test-runner
                #:ensure-test-runner
                #:request-to-run-test
                #:request-to-run-test*)
  (:import-from #:breeze.xref
                #:calls-who
                #:test-calls-who
                #:tested-by
                #:test-case
		#:find-packages-by-prefix)
  (:export
   ;; definition
   #:defun
   #:fmakunbound
   #:function-body
   ;; test
   #:deftest
   #:is
   #:run-test
   #:run-all-tests
   #:test-body
   #:selftest
   ;; xref
   #:calls-who
   #:test-calls-who
   #:tested-by
   #:test-case
   ;; MAIN
   #:main
   #:next))

(in-package #:breeze.user)

(cl:defun run-test-for-function (function-name)
  (ensure-test-runner)
  (request-to-run-test* (tested-by function-name)))


(defun welcome ()
  ;; figlet -mini breeze
  (format t "~&~%~A~%~%" "
    |_ .__  _ _  _
    |_)|(/_(/_/_(/_ ")

  (format t "~%Tips:~%")
  (format t "~&~{ * ~A~%~}"
          '(#+later "Remember to use the emacs mode if applicable."
            "Use \"br\" as a nickname for \"breeze.user\" (e.g. `br:main` instead of `breeze.user:main`)."
            "Use (require 'swank) followed by (swank:create-server) to start swank.")))

(defun main ()
  "Call this function to start."
  (pushnew 'run-test-for-function *function-redifinition-hooks*)
  (pushnew 'request-to-run-test *test-change-hooks*)
  (welcome))

(defun next ()
  "Call this to get hints on what to do next."
  (let ((missing-documentation-in-breeze
	 (loop :for package :in (find-packages-by-prefix "breeze")
	    :append (find-undocumented-symbols package))))
    (when missing-documentation-in-breeze
      (princ "There are undocumented symbols in breeze's packages:")
      (format t "~&~{ * ~A~%~}"
	      missing-documentation-in-breeze))))

(defun selftest ()
  "Load and run breeze's selftests."
  (load (merge-pathnames "tests/selftest.lisp"
			 (breeze.asdf:system-directory '#:breeze)))
  (uiop:symbol-call '#:breeze.selftest '#:selftest))
