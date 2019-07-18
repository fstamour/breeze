
(asdf:defsystem #:breeze
  :name "breeze"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "A system to help automate work."
  :depends-on (#:uiop #:alexandria #:chanl #:anaphora)
  :serial t
  :components
  ((:module "src"
    :components
            ((:file "utils")
             (:file "definition")
             (:file "test")
             (:file "test-runner")
             (:file "xref")
             (:file "user")))
   (:module "tests"
    :components
            ((:file "selftest")))))
