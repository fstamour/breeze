(defpackage #:breeze.test-commands
  (:documentation "Commands related to tests")
  (:use #:cl #:breeze.command)
  (:export #:run-test-at-point
           #:run-tests-in-file
           #:run-tests-in-suite
           #:run-system-tests
           #:run-all-tests
           #:quickfix-test))

(in-package #:breeze.test-commands)

#|

ideas:
- debug tests
  - maybe implicitely insert breaks
- generate tests
- re-run tests on change (watch)
  - use coverage data (from a full-run) to know which tests to re-run
  - command: toggle watch
  - command: toggle watch for specific tests or suite
- run with coverage
- show test "status" in the editor (e.g. in emacs' header-line or mode-line)
- run in a different process!
- mutation testing

it's possible to narrow down which test is probably points to the
right root cause when a lot of tests fails by inspecting the relations
of the functions being called by the tests (i.e. infer the
dependencies between the tests).

|#

;; TODO insert-test-system-definition

;; TODO  defmethods
;; TODO support multiple test framework _at the same time_


;;; Functions for working with test frameworks

(defvar *known-test-frameworks*
  '(1am
    cacau
    check-it
    checkl
    cl-naive-test
    cl-quickcheck
    clite
    clue #| used by alive, not in quicklisp https://github.com/nobody-famous/clue |#
    clunit
    clunit2
    fiasco
    fiveam
    lift
    lisp-unit
    lisp-unit2
    monkeylib-test-framework
    parachute
    parten-test
    ptester
    rove
    prove #| archived in 2020 |#
    rt
    should-test
    simplet
    stefil
    test-urils
    testiere
    try
    unit-test
    xlunit))

;; protest

(defun detect-all-test-framework ())


;;; Commands for working with tests

(define-command run-test-at-point ()
  "Run the test at point."
  'not-implemented-yet)

(define-command run-tests-in-file ()
  "Run the all the tests in a file."
  'not-implemented-yet)

(define-command run-tests-in-suite ()
  "Run the all the tests in a suite."
  'not-implemented-yet)

(define-command run-system-tests ()
  "Run a system's tests using ~(asdf:test-system)~."
  'not-implemented-yet)

(define-command run-all-tests ()
  "Run all the tests..."
  'not-implemented-and-not-quite-sure-exactly-what-this-should-do)

(define-command quickfix-test ()
  "Tries to fix the test(s) automagically."
  'not-implemented
  #| If the assertion is not complete, run the code and try to
  generate the missing part (the "expected" value). |#)

(define-command goto-test ()
  "Go to a specific test."
  'not-implemented)

(define-command undefine-test-at-point ()
  "Remove the test at point"
  'not-implemented)
