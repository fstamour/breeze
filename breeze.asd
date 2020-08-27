
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
	       #:staple
	       #:quickproject
	       #:swank
	       #:uiop)
  :serial t
  :components
  ((:module "src"
	    :components
	    ((:file "utils")
	     (:file "documentation")
	     (:file "definition")
	     (:file "test")
	     (:file "worker")
	     (:file "test-runner")
	     (:file "xref")
	     (:file "asdf")
	     (:file "breeze-swank")
	     (:file "user")))
   ;; TODO move this into its own system (breeze.selftest.asd)
   (:module "tests"
	    :components
            ((:file "user")
	     (:file "xref")))))
