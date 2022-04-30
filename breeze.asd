
(defpackage #:breeze.asd
  (:documentation "Package containing breeze's system defintions")
  (:use :cl :asdf))

(in-package #:breeze.asd)

(asdf:defsystem #:breeze/config
  :description "Configurations for breeze."
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :pathname "src"
  :serial t
  :components
  ((:file "configuration")))


(defsystem #:breeze
  :name "breeze"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "A system to help automate work."
  :depends-on (breeze/config
               ;; Multi-threading
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
               ;; #:swank
               ;; Logging
               #:log4cl)
  :pathname "src"
  :components
  ((:file "cl")
   (:file "utils")
   (:file "syntax-tree")
   (:file "reader" :depends-on ("syntax-tree" "utils"))
   (:file "command")
   (:file "asdf")
   (:file "xref" :depends-on ("utils"))
   (:file "documentation" :depends-on ("xref"))
   (:file "doctor")
   ;; (:file "breeze-listener" :depends-on ("xref"))
   (:file "refactor" :depends-on ("reader" "command" "utils" "cl"))
   (:file "project" :depends-on ("utils" "command"))
   (:file "capture" :depends-on ("utils" "command")))
  :in-order-to ((test-op (load-op #:breeze/test)))
  :perform
  (test-op (o c)
           (uiop:symbol-call
            '#:breeze.test.main '#:run-breeze-tests)))


(defsystem "breeze/test"
  :description "Tests for the breeze system."
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
   (:file "refactor")
   (:file "dummy-package")
   (:file "xref")
   (:file "documentation")
   (:file "main")))
