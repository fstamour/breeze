
(defpackage #:breeze.asd
  (:use :cl :asdf))

(in-package #:breeze.asd)

(defsystem #:breeze
  :name "breeze"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "A system to help automate work."
  :depends-on (;; Multi-threading
	       #:bordeaux-threads
	       #:chanl
	       ;; To create projects
	       #:quickproject
	       ;; Utilities
	       #:alexandria
	       #:anaphora
	       #:cl-hash-util
	       #:cl-ppcre
	       #:closer-mop
	       #:str
	       #:uiop
	       ;; For documentation generation
	       #:3bmd #:3bmd-ext-code-blocks #:3bmd-ext-tables #:spinneret
	       ;; For reading lisp
	       #:eclector
	       #:trivial-package-local-nicknames
	       ;; Listener(s) stuff
	       #:swank
	       ;; Logging
	       #:log4cl)
  :pathname "src"
  :components
  ((:file "utils" :depends-on ())
   (:file "reader" :depends-on ("utils"))
   (:file "definition" :depends-on ())
   (:file "test" :depends-on ())
   (:file "worker")
   (:file "test-runner" :depends-on ("test" "worker"))
   (:file "xref" :depends-on ("utils" "test" "definition"))
   (:file "documentation" :depends-on ("xref" "definition"))
   (:file "asdf" :depends-on ())
   (:file "breeze-listener" :depends-on ("xref"))
   (:file "command" :depends-on ())
   (:file "refactor" :depends-on ("reader" "command" "utils"))
   (:file "user" :depends-on ("test-runner"
			      "xref"
			      "documentation"
			      "refactor"
			      "asdf")))
  :in-order-to ((test-op (load-op #:breeze/test)))
  :perform
  (test-op (o c)
	   (uiop:symbol-call
	    '#:breeze.test.main '#:run-breeze-tests)))


(defsystem "breeze/test"
  :description ""
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (#:breeze #:parachute)
  :pathname "tests"
  :serial t
  :components
  ((:file "utils")
   (:file "reader")
   (:file "command")
   (:file "dummy-package")
   (:file "test")
   (:file "user")
   (:file "xref")
   (:file "documentation")
   (:file "main")))
