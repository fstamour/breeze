
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
		#:find-packages-by-prefix
		#:find-packages-by-regex)
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
   #:start-test-runner
   #:stop-test-runner
   #:ensure-test-runner
   ;; xref
   #:calls-who
   #:test-calls-who
   #:tested-by
   #:test-case
   ;; MAIN
   #:main
   #:next
   #:*current-packages*
   #:current-packages))

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
            "Use (require 'swank) followed by (swank:create-server) to start swank."
	    "Once swank is started, call (breeze.swank:advise-swank-interactive-eval)"
	    "Use (br:ensure-test-runner) or (br:start-test-runner) to be able to run tests automatically in the background.")))

(defun main ()
  "Call this function to start."
  (pushnew 'run-test-for-function *function-redifinition-hooks*)
  (pushnew 'request-to-run-test *test-change-hooks*)
  (welcome))

;; Naming is hard
(defun package-and-siblings ()
  "If *package* contains a dot \".\", returns all packages with the same prefix, else return the current package."
  (let ((name (package-name *package*)))
    (alexandria:if-let
	(pos (position #\. name))
      (find-packages-by-prefix (subseq name 0 pos))
      *package*)))

(defparameter *current-packages*
  #'package-and-siblings
  "Specify which packages you are working on. By default this variable
  is set to a function that returns the current value of *package*.")

(defun current-packages (&optional (package-designator *current-packages*))
  "Coerce *current-packages* into a list of packages."
  (alexandria:ensure-list
   (cond
     ((packagep package-designator)
      package-designator)
     ((symbolp package-designator)
      (find-package package-designator))
     ((listp package-designator)
      (loop :for designator :in package-designator
	 :append (current-packages designator)))
     ((when (functionp package-designator)
	(funcall package-designator))))))

(defun cheers ()
  "Bravo!")

(defun check-for-undocumented-symbols ()
  (let ((missing-documentation
	 (loop
	    :for package :in (current-packages)
	    :append (find-undocumented-symbols package))))
    (if missing-documentation
	(progn
	  (princ "There are undocumented symbols in current packages:")
	  (format t "~&~{ * ~A~%~}"
		  missing-documentation))
	(format t "~&No undocumented symbols found. ~A" (cheers)))))

(defun next ()
  "Call this to get hints on what to do next."
  (check-for-undocumented-symbols))

(defun selftest ()
  "Load and run breeze's selftests."
  (breeze.test:run-all-tests)
  (load (merge-pathnames "tests/selftest.lisp"
			 (breeze.asdf:system-directory '#:breeze)))
  (uiop:symbol-call '#:breeze.test.test '#:run-all-selftest)
  ;; TODO Run only breeze's tests (hint: find-packages-by-prefix)
  )
