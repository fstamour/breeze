
(defpackage #:breeze.asd
  (:documentation "Package containing breeze's system defintions")
  (:use :cl :asdf))

(in-package #:breeze.asd)

(asdf:defsystem breeze/config
  :description "Configurations for breeze."
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :pathname "src"
  :serial t
  :components
  ((:file "configuration")))


(defsystem breeze
  :name "breeze"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "A system to help automate work."
  :depends-on (breeze/config
               ;; Multi-threading
               bordeaux-threads
               chanl
               trivial-timeout
               ;; To create projects
               quickproject
               ;; Utilities
               alexandria
               anaphora
               ;; cl-hash-util
               cl-ppcre
               closer-mop
               str
               uiop
               ;; For documentation generation
               3bmd 3bmd-ext-code-blocks 3bmd-ext-tables spinneret
               ;; For reading lisp
               eclector
               trivial-package-local-nicknames
               ;; For some portability checks
               trivial-features)
  :pathname "src"
  :components
  ((:file "logging")
   (:file "cl")
   (:file "utils")
   ;; TODO #++
   (:file "syntax-tree")
   #++
   (:file "reader" :depends-on ("syntax-tree" "utils"))
   (:file "lossless-reader" :depends-on ("utils"))
   (:file "pattern")
   (:file "command"
    :depends-on (#++"reader"
                 #++"syntax-tree"
                 "utils"))
   (:file "asdf")
   (:file "thread" :depends-on ("xref"))
   (:file "xref" :depends-on ("utils"))
   (:file "documentation" :depends-on ("xref"))
   (:file "doctor")
   (:file "listener"
    :depends-on ("xref"
                 "command"))
   (:file "refactor" :depends-on (#++"reader" "command" "utils" "cl"))
   (:file "project" :depends-on ("utils" "command"))
   (:file "capture" :depends-on ("utils" "command")))
  :in-order-to ((test-op (load-op breeze/test)))
  :perform
  (test-op (o c)
           (uiop:symbol-call
            'breeze.test.main 'run-breeze-tests)))

(defsystem breeze/docs
  :description "Breeze component to generate documentation."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (breeze)
  :pathname "src"
  :serial t
  :components
  ((:file "documentation")))

(defsystem "breeze/kite"
  :description "A breeze in a parachute makes a kite: utils for parachute"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (parachute breeze)
  :pathname "kite"
  :serial t
  :components
  ((:file "kite")))

(defsystem "breeze/test"
  :description "Tests for the breeze system."
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (breeze parachute breeze/kite)
  :pathname "tests"
  :serial t
  :components
  ((:file "utils")
   (:file "logging")
   #++
   (:file "reader")
   (:file "lossless-reader")
   (:file "pattern")
   (:file "command")
   (:file "refactor")
   (:file "dummy-package")
   (:file "xref")
   (:file "documentation")
   (:file "main")))
