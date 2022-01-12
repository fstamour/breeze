
(asdf:defsystem #:breeze
  :name "breeze"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "A system to help automate work."
  :depends-on (#:alexandria
	       #:anaphora
	       #:chanl
	       #:cl-hash-util
	       #:cl-ppcre
	       #:closer-mop
	       #:quickproject
	       #:str
	       #:swank
	       #:uiop
	       ;; For documentation generation
	       #:3bmd #:3bmd-ext-code-blocks #:3bmd-ext-tables #:spinneret
	       #:eclector)
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
   (:file "breeze-swank" :depends-on ("xref"))
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
	   (symbol-call
	    '#:breeze.user '#:selftest)))


(defsystem "breeze/test"
  :description ""
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (#:breeze)
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
   (:file "documentation")))
