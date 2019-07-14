
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
  (run-all-tests (tested-by function-name)))

(pushnew 'run-test-for-function *function-change-hooks*)

(pushnew 'run-test *test-change-hooks*)

(print "Tip: Remember to use the emacs mode if applicable.")
