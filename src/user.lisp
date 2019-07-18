
(uiop:define-package #:breeze.user
    (:reexport :cl)
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
   #:test-case))

(in-package #:breeze.user)

(cl:defun run-test-for-function (function-name)
  (ensure-test-runner)
  (request-to-run-test* (tested-by function-name)))

(pushnew 'run-test-for-function *function-change-hooks*)
(pushnew 'request-to-run-test *test-change-hooks*)

(defun welcome ()
  ;; figlet -mini breeze
  (format t "~&~%~A~%~%" "
    |_ .__  _ _  _
    |_)|(/_(/_/_(/_ ")

  (format t "~%Tips:~%")
  (format t "~&~{~A~%~}"
          '(" * Remember to use the emacs mode if applicable."
            " * Use (require 'swank) followed by (swank:create-server) to start swank.")))

(welcome)

