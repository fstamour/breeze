
(in-package #:common-lisp-user)

(defpackage #:try-docparser
  (:use :cl))

(in-package #:try-docparser)

(ql:quickload 'docparser)

(defparameter *idx*
  (docparser:parse :breeze))

(docparser:dump *idx*)
#|
Package "BREEZE.UTILS" with docstring "Utilities"
  #<function walk (TREE FN &OPTIONAL (RECURSE-P (CONSTANTLY T)))>
  #<function walk-list (TREE FN &OPTIONAL (RECURSE-P (CONSTANTLY T)))>
  #<function walk-car (TREE FN &OPTIONAL (RECURSE-P (CONSTANTLY T)))>
  #<function package-apropos (SEARCH-STRING)>
  #<function optimal-string-alignment-distance (VEC-A VEC-B)>
  #<function indent-string (INDENTATION STRING)>
  #<function print-comparison (STREAM STRING1 STRING2)>
Package "BREEZE.DEFINITION" with docstring "Provides replacements for \"definition forms\" (such as defun and defmacro).
The goal is to (portably) make sure we keep the definitions and not just their [compiled] results."
  #<variable *function*>
  #<variable *function-redifinition-hooks*>
  #<function flag-funtion-redifinition (NAME)>
  #<macro defun (&WHOLE WHOLE NAME LAMBDA-LIST &BODY BODY)>
  #<function fmakunbound (NAME)>
  #<function function-body (NAME)>
Package "BREEZE.TEST" with docstring "Provides a test framework."
  #<variable *test*>
  #<variable *test-change-hooks*>
  #<variable *test-output-stream*>
  #<function flag-test-change (NAME)>
  #<function flag-failed-test (NAME BODY CONDITION)>
  #<function make-test (NAME PACKAGE BODY)>
  #<macro deftest (NAME &BODY BODY)>
  #<function test-body (NAME)>
  #<function run-test (NAME)>
  #<function test (NAME &OPTIONAL (MESSAGE-ON-SUCCESS ~&Passed.))>
  #<function run-all-tests (&OPTIONAL TEST-LIST)>
  #<macro is (&BODY BODY)>
Package "BREEZE.WORKER" with docstring "Worker class and methods"
  #<class worker>
  #<generic function worker-input-channel (WORKER)>
  #<generic function worker-output-channel (WORKER)>
  #<generic function worker-interval (WORKER)>
  #<generic function worker-receive-all-messages (WORKER)>
  #<method worker-receive-all-messages ((WORKER WORKER))>
  #<generic function worker-report (WORKER LEVEL CONTROL-STRING &REST
                                  FORMAT-ARGUMENTS)>
  #<method worker-report ((WORKER WORKER) LEVEL CONTROL-STRING &REST
                        FORMAT-ARGUMENTS)>
  #<generic function worker-process-messages (WORKER MESSAGE-LIST)>
  #<function worker-set-last-iteration-time (WORKER)>
  #<generic function worker-run (WORKER)>
  #<method worker-run ((WORKER WORKER))>
  #<generic function worker-control-loop (WORKER)>
  #<method worker-control-loop ((WORKER WORKER))>
  #<generic function worker-start (WORKER)>
  #<method worker-start ((WORKER WORKER))>
  #<generic function worker-stop (WORKER)>
  #<method worker-stop ((WORKER WORKER))>
  #<generic function worker-alive-p (WORKER)>
  #<method worker-alive-p ((WORKER WORKER))>
  #<generic function worker-ensure-alive (WORKER)>
  #<method worker-ensure-alive ((WORKER WORKER))>
  #<generic function worker-send (WORKER MESSAGE)>
  #<method worker-send ((WORKER WORKER) MESSAGE)>
  #<method worker-report ((TEST-RUNNER TEST-RUNNER) LEVEL CONTROL-STRING &REST
                        FORMAT-ARGUMENTS)>
  #<method worker-run ((TEST-RUNNER TEST-RUNNER))>
Package "BREEZE.TEST-RUNNER" with docstring "Provides a test-runner (and methods to interact
with it).  Alternatively, you _could_ run many different
test-runner."
  #<class test-runner>
  #<variable *test-runner*>
  #<function process-messages (MESSAGE-LIST)>
  #<function receive-messages NIL>
  #<function start-test-runner NIL>
  #<function stop-test-runner NIL>
  #<function ensure-test-runner NIL>
  #<function request-to-run-test (TEST)>
  #<function request-to-run-test* (TEST-LIST)>
Package "BREEZE.XREF" with docstring "Cross-reference and introspection"
  #<function calls-who (FUNCTION-NAME)>
  #<function test-calls-who (TEST-NAME)>
  #<function tested-by (FUNCTION-NAME)>
  #<function test-case (FUNCTION-NAME)>
  #<function function-without-test (&OPTIONAL (PACKAGE *PACKAGE*))>
  #<function find-packages-by-prefix (PREFIX)>
  #<function find-packages-by-regex (REGEX &OPTIONAL (CASE-INSENSITIVE-P T))>
  #<function generic-method-p (SYMBOL)>
  #<function specialp (SYMBOL)>
  #<function simple-function-p (SYMBOL)>
Package "BREEZE.DOCUMENTATION" with docstring "Tools to inspect and generate documentation"
NIL
|#

(docparser:query *idx* :package-name "BREEZE.XREF")
#| =>
#(#<function calls-who (FUNCTION-NAME)> #<function test-calls-who (TEST-NAME)>
  #<function tested-by (FUNCTION-NAME)> #<function test-case (FUNCTION-NAME)>
  #<function function-without-test (&OPTIONAL (PACKAGE *PACKAGE*))>
  #<function find-packages-by-prefix (PREFIX)>
  #<function find-packages-by-regex (REGEX &OPTIONAL (CASE-INSENSITIVE-P T))>
  #<function generic-method-p (SYMBOL)> #<function specialp (SYMBOL)>
  #<function simple-function-p (SYMBOL)>)
|#

(docparser:query *idx*
		 :package-name "BREEZE.DUMMY.TEST"
		 :symbol-name "FUNCTION-DOCUMENTED")
;; Not what I expected :/
;; => #()


(docparser:do-packages (package *idx*)
  )
