
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
	       #:cl-ppcre
	       #:closer-mop
	       #:quickproject
	       #:swank
	       #:uiop
	       #:str
	       ;; For documentation generation
	       #:3bmd #:3bmd-ext-code-blocks #:3bmd-ext-tables #:spinneret
	       )
  :serial t
  :components
  ((:module "src"
	    :components
	    ((:file "utils")
	     (:file "definition")
	     (:file "test")
	     (:file "worker")
	     (:file "test-runner")
	     (:file "xref")
	     (:file "documentation")
	     (:file "asdf")
	     (:file "breeze-swank")
	     (:file "user")))
   ;; TODO move this into its own system (breeze.selftest.asd)
   (:module "tests"
	    :components
	    ((:file "user")
	     (:file "dummy-package")
	     (:file "xref")
	     (:file "documentation")))))
