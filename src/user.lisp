
(uiop:define-package #:breeze.user
    (:use :cl)
  (:documentation "Use this package.")
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
		#:package-test
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
   ;; documentation
   #:find-undocumented-symbols
   ;; xref
   #:calls-who
   #:test-calls-who
   #:tested-by
   #:test-case
   ;; MAIN
   #:main
   #:next
   #:*current-packages*
   #:current-packages
   #:dogfood))

(in-package #:breeze.user)

(cl:defun run-test-for-function (function-name)
  (ensure-test-runner)
  (request-to-run-test* (tested-by function-name)))

(defun maybe-tips-about-test-runner (&optional (stream *standard-output*))
  (unless (breeze.worker:worker-alive-p
	   breeze.test-runner::*test-runner*)
    (format stream "~&Use (br:ensure-test-runner) or (br:start-test-runner) to be able to run tests automatically in the background.")))

(defun welcome ()
  ;; figlet -mini breeze
  (format t "~&~%~A~%~%" "
    |_ .__  _ _  _
    |_)|(/_(/_/_(/_ ")

  (format t "~%Tips:~%")
  (format t "~&~{ * ~A~%~}"
	  (remove-if #'null
		     `(#+later "Remember to use the emacs mode if applicable."
			       "User (br:next) to get hints on what to do next."
			       "Use \"br\" as a nickname for \"breeze.user\" (e.g. `br:main` instead of `breeze.user:main`)."
			       "Use (require 'swank) followed by (swank:create-server) to start swank."
			       "Once swank is started, call (breeze.swank:advise-swank-interactive-eval)"
			       "User (br:dogfood) to start hacking on breeze."
			       ,(maybe-tips-about-test-runner nil)))))

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
     ((stringp package-designator)
      (find-packages-by-regex package-designator))
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
	  (format t "~&There are undocumented symbols in current packages:")
	  (format t "~&~{ * ~A~%~}"
		  missing-documentation))
	(format t "~&No undocumented symbols found. ~A" (cheers)))))

(defun next ()
  "Call this to get hints on what to do next."
  (maybe-tips-about-test-runner)
  (check-for-undocumented-symbols))

(defun selftest ()
  "Load and run breeze's selftests."
  (terpri)
  (loop
     :for package :in (current-packages "^breeze\\.[^.]+.test$")
     :do
       (format t "~&Testing package \"~(~A~)\"." (package-name package))
       (run-all-tests (package-test package))))

(defun dogfood ()
  "Setup breeze to work on breeze."
  (breeze.swank:advise-swank-interactive-eval)
  (setf breeze.user:*current-packages*  "^breeze\\.[^.]+$")
  (ensure-test-runner))
