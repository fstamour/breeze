
(defpackage #:breeze.try-40ants-ci
  (:use #:cl)
  (:documentation ""))

(in-package #:breeze.try-40ants-ci)

(ql:system-apropos-list "40ants")
#|
(#<QL-DIST:SYSTEM 40ants-doc / doc-20210630-git / quicklisp 2021-08-07>
#<QL-DIST:SYSTEM 40ants-doc-full / doc-20210630-git / quicklisp 2021-08-07>
#<QL-DIST:SYSTEM 40ants-doc-test / doc-20210630-git / quicklisp 2021-08-07>)
|#

;;; cloned https://github.com/40ants/ci in local-projects

(ql:quickload "40ants-ci")

;;; System "docs-config" not found, it needs more stuff (that are in
;;; UltraLisp...)


(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)

;; Let's try again
(ql:quickload "40ants-ci")


(40ants-ci/workflow:defworkflow test
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)))

;;; So, it's trying to find a system with the same name as the package
;;; Which is was the doc says: "In next examples, I'll presume you are
;;; writing code in a file which is the part of the package inferred
;;; ASDF system [...]"

#|
Component "breeze.try-40ants-ci" not found
   [Condition of type ASDF/FIND-COMPONENT:MISSING-COMPONENT]

Restarts:
 0: [RETRY] Retry ASDF operation.
 1: [CLEAR-CONFIGURATION-AND-RETRY] Retry ASDF operation after resetting the configuration.
 2: [ABORT] Abort compilation.
 3: [*ABORT] Return to SLIME's top level.
 4: [ABORT] abort thread (#<THREAD "worker" RUNNING {101EC80F23}>)

Backtrace:
  0: ((LAMBDA NIL :IN ASDF/SYSTEM:FIND-SYSTEM))
  1: (ASDF/SESSION:CONSULT-ASDF-CACHE (ASDF/SYSTEM:FIND-SYSTEM "breeze.try-40ants-ci") #<FUNCTION (LAMBDA NIL :IN ASDF/SYSTEM:FIND-SYSTEM) {101ECA295B}>)
  2: (ASDF/SESSION:CALL-WITH-ASDF-SESSION #<FUNCTION (LAMBDA NIL :IN ASDF/SYSTEM:FIND-SYSTEM) {101ECA295B}> :OVERRIDE NIL :KEY (ASDF/SYSTEM:FIND-SYSTEM "breeze.try-40ants-ci") :OVERRIDE-CACHE NIL :OVERRIDE..
  3: ((:METHOD 40ANTS-CI/WORKFLOW::ON-WORKFLOW-REDEFINITION (40ANTS-CI/WORKFLOW::WORKFLOW)) #<TEST {101ECA1923}>) [fast-method]
  4: (SB-FASL::LOAD-FASL-GROUP #S(SB-FASL::FASL-INPUT :STREAM #<SB-SYS:FD-STREAM for "file /tmp/slimeXANLXZ.fasl" {101EC99E23}> :TABLE #(47 #<PACKAGE "SB-PCL"> SB-PCL::LOAD-DEFCLASS #<PACKAGE "BREEZE.TRY-4..
  5: (SB-FASL::LOAD-AS-FASL #<SB-SYS:FD-STREAM for "file /tmp/slimeXANLXZ.fasl" {101EC99E23}> NIL NIL)
  6: ((LABELS SB-FASL::LOAD-STREAM-1 :IN LOAD) #<SB-SYS:FD-STREAM for "file /tmp/slimeXANLXZ.fasl" {101EC99E23}> T)
  7: (SB-FASL::CALL-WITH-LOAD-BINDINGS #<FUNCTION (LABELS SB-FASL::LOAD-STREAM-1 :IN LOAD) {804DFF00B}> #<SB-SYS:FD-STREAM for "file /tmp/slimeXANLXZ.fasl" {101EC99E23}> T #<SB-SYS:FD-STREAM for "file /tmp..
  8: (LOAD #P"/tmp/slimeXANLXZ.fasl" :VERBOSE NIL :PRINT NIL :IF-DOES-NOT-EXIST T :EXTERNAL-FORMAT :DEFAULT)
  9: ((FLET SWANK/BACKEND:CALL-WITH-COMPILATION-HOOKS :IN "/usr/home/fstamour/.config/stumpwm/slime/swank/sbcl.lisp") #<FUNCTION (LAMBDA NIL :IN SWANK/BACKEND:SWANK-COMPILE-STRING) {101EC99CDB}>)
 10: ((FLET SWANK/BACKEND:SWANK-COMPILE-STRING :IN "/usr/home/fstamour/.config/stumpwm/slime/swank/sbcl.lisp") "(40ants-ci/workflow:defworkflow test ..)
 11: ((LAMBDA NIL :IN SWANK:COMPILE-STRING-FOR-EMACS))
 12: ((LAMBDA NIL :IN SWANK::COLLECT-NOTES))
 13: (SWANK::MEASURE-TIME-INTERVAL #<FUNCTION (LAMBDA NIL :IN SWANK::COLLECT-NOTES) {101EC907DB}>)
 14: (SWANK::COLLECT-NOTES #<FUNCTION (LAMBDA NIL :IN SWANK:COMPILE-STRING-FOR-EMACS) {101EC9078B}>)
 15: (SWANK::CALL-WITH-BUFFER-SYNTAX NIL #<FUNCTION (LAMBDA NIL :IN SWANK:COMPILE-STRING-FOR-EMACS) {101EC9073B}>)
 16: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SWANK:COMPILE-STRING-FOR-EMACS "(40ants-ci/workflow:defworkflow test ..)
 17: (EVAL (SWANK:COMPILE-STRING-FOR-EMACS "(40ants-ci/workflow:defworkflow test ..)
 18: (SWANK:EVAL-FOR-EMACS (SWANK:COMPILE-STRING-FOR-EMACS "(40ants-ci/workflow:defworkflow test ..)
 19: ((LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD))
 20: (SWANK/SBCL::CALL-WITH-BREAK-HOOK #<FUNCTION SWANK:SWANK-DEBUGGER-HOOK> #<FUNCTION (LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD) {2273525B}>)
 21: ((FLET SWANK/BACKEND:CALL-WITH-DEBUGGER-HOOK :IN "/usr/home/fstamour/.config/stumpwm/slime/swank/sbcl.lisp") #<FUNCTION SWANK:SWANK-DEBUGGER-HOOK> #<FUNCTION (LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD..
 22: (SWANK::CALL-WITH-BINDINGS ((*STANDARD-INPUT* . #<SWANK/GRAY::SLIME-INPUT-STREAM {101880D173}>)) #<FUNCTION (LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD) {227354CB}>)
 23: ((LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD))
 24: ((FLET SB-UNIX::BODY :IN SB-THREAD::RUN))
 25: ((FLET "WITHOUT-INTERRUPTS-BODY-11" :IN SB-THREAD::RUN))
 26: ((FLET SB-UNIX::BODY :IN SB-THREAD::RUN))
 27: ((FLET "WITHOUT-INTERRUPTS-BODY-4" :IN SB-THREAD::RUN))
 28: (SB-THREAD::RUN)
 29: ("foreign function: call_into_lisp")
 30: ("foreign function: funcall1")
|#


;;; Lookint at the macro expension
;;; - it creates class named TEST (the name of the workflow)
;;; - it makes a list of object of type Job, then
;;; - create an instance of the class TEST
;;; - register that instance
;;; - calls the method on-workflow-redefinition with that instance
;;;
;;; It's the last call (on-workflow-redefinition) that trips my test
;;; here. The code heavily assume that you use ASDF's
;;; package-inferred-system.


;;; We can still reach into 40ants/ci's guts

(defparameter *wf*
  (gethash 'test
	   (gethash (package-name *package*)
		    40ants-ci/workflow::*registered-workflows*))
"The workflow object that was just created, but failed to generate (in on-workflow-redefinition).")

(let ((40ants-ci/vars:*current-system* (asdf:find-system 'breeze)))
  (40ants-ci/github:generate
   *wf*
   (breeze.utils:breeze-relative-pathname ".github/workflows/linter.yml")))


;;; ^ That hacky stuff worked, let's try something better
;;; I'm getting tired, so the prose is going away.

(defgeneric workflow-system (workflow))

(defmethod workflow-system ((workflow 40ants-ci/workflow::workflow))
  (asdf:find-system
   (asdf:primary-system-name
    (string-downcase
     (package-name *package*)))))

(defclass system-based-workflow () ; not a good name...
  ((system :reader workflow-system)))

(defclass breeze-workflow (system-based-workflow)
  ((system :initform (asdf:find-system 'breeze))))

(defclass linter (breeze-workflow) ())

(workflow-system (make-instance 'breeze-workflow))
;; => #<ASDF/SYSTEM:SYSTEM "breeze">

(defun on-workflow-redefinition* (workflow)
    (let* ((system (print (breeze.try-40ants-ci::workflow-system workflow) *debug-io*))
	   (system-path (asdf:system-relative-pathname system ""))
	   (40ants-ci/vars:*current-system* system)
	   (workflow-path
	     (40ants-ci/workflow::make-workflow-path system-path workflow)))
      (40ants-ci/github:generate workflow workflow-path)
      ))

(defmacro defworkflow (name &key
                              on-push-to
                              by-cron
                              on-pull-request
                              cache
                              jobs
			      superclasses)
  `(progn
     (defclass ,name (workflow ,@superclasses)
       ())
     (let* ((jobs (mapcar #'make-job ',jobs))
            (workflow (make-instance ',name
                                     :name ',name
                                     :jobs jobs
                                     :on-push-to ',(uiop:ensure-list on-push-to)
                                     :by-cron ',(uiop:ensure-list by-cron)
                                     :on-pull-request ,on-pull-request
                                     :cache ,cache)))
       (register-workflow workflow)
       (on-workflow-redefinition* workflow)
       workflow)))


(in-package #:breeze.try-40ants-ci)

(40ants-ci/workflow:defworkflow linter
  :superclasses (breeze-workflow)
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)))

;;; For some reason (that I'm too tired to figure out), it is still
;;; calling the original on-workflow-redefinition... I should check
;;; the call sack to see if it's called from somewhere else... But it
;;; didn't work even when I had redefined the exact same method (same
;;; specializers).

;;; Uninstall ultralisp
#+ (or)
(ql-dist:uninstall
 (find-if #'(lambda (dist)
	      (string= "ultralisp"
		       (ql-dist:name dist)))
	  (ql-dist:all-dists)))
