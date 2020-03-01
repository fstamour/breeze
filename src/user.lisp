
(uiop:define-package #:breeze.user
    (:reexport :cl)
  (:documentation "This package is meant to be used by the end user, it re-export everything from cl.")
  (:nicknames :br :br-user :breeze)
  (:shadowing-import-from #:breeze.definition
                          #:defun
                          #:fmakunbound)
  (:import-from #:breeze.definition
                #:*function-change-hooks*
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
                #:test-case)
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
   ;; xref
   #:calls-who
   #:test-calls-who
   #:tested-by
   #:test-case
   ;; MAIN
   #:main))

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
  (pushnew 'run-test-for-function *function-change-hooks*)
  (pushnew 'request-to-run-test *test-change-hooks*)
  (welcome))

