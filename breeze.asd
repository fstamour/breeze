;;;; System definitions for breeze and auxiliary systems

(in-package #:asdf-user)


;;; breeze system

(defsystem breeze/asdf
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "Utilities for asdf"
  :depends-on (asdf
               ;; as of 2025-10-31 — only for "flatten"
               alexandria
               uiop)
  :pathname "src/"
  :components ((:file "asdf")))

(defsystem breeze/generics
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "Generic functions used across breeze's systems."
  :pathname "src/"
  :components ((:file "generics")))

(defsystem breeze/class-utils
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "Utility macros for defining classes along with common methods."
  :pathname "src/"
  :components ((:file "class-utils")))

(defsystem breeze/pattern
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "Pattern matching with first-class patterns"
  :depends-on (;; as of 2025-10-31 — only for "flatten"
               alexandria
               breeze/generics
               breeze/class-utils)
  :pathname "src/pattern/"
  :serial t
  :components ((:file "iterator")
               (:file "pattern")
               (:file "compile-pattern")
               (:file "substitution")
               (:file "match")
               (:file "rewrite")))

(defsystem breeze
  :name "breeze"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "A system to help automate work."
  :depends-on (;; Multi-threading
               bordeaux-threads
               ;; Utilities
               alexandria
               uiop
               ;; cl-heap
               breeze/generics
               breeze/class-utils
               breeze/pattern)
  :pathname "src/"
  :components
  ((:file "logging")
   (:file "cl")
   (:file "utils")
   (:file "indirection")
   (:file "string-utils" :depends-on ("utils"))
   (:file "channel")
   ;; "test-file" is for parsing ERT files
   (:file "test-file" :depends-on ("utils" "string-utils"))
   (:file "configuration")
   (:file "range")
   (:module "parser"
    :depends-on ("range" "string-utils")
    :serial t
    :components ((:file "parser-state")
                 (:file "parse-tree")
                 (:file "parser")
                 (:file "incremental-parser")))
   (:file "buffer" :depends-on ("parser" "package"))
   (:file "workspace" :depends-on ("parser" "buffer"))
   (:file "egraph")
   (:file "analysis" :depends-on ("parser"))
   (:file "command"
    :depends-on ("utils"
                 "configuration"
                 "indirection"
                 ;; for externalp
                 "xref"))
   (:file "thread" :depends-on ("xref"))
   (:file "xref" :depends-on ("utils"))
   (:file "doctor")
   (:file "listener"
    :depends-on ("xref" "command"))
   (:file "suggestion"
    :depends-on ("listener"))
   (:file "package" :depends-on ("analysis"))

   (:file "lint" :depends-on ("analysis" "command"))
   (:file "refactor" :depends-on ("cmds" "cl"
                                         "indirection"
                                         "workspace"))
   (:file "cmds/command-utils")
   (:module "cmds"
    :depends-on ("command" "cmds/command-utils"
                           "analysis"
                           "configuration" "utils")
    :components ((:file "blueprint")
                 (:file "completion")
                 (:file "editing")
                 (:file "project")
                 (:file "capture")
                 (:file "package-commands")
                 (:file "test-commands")
                 (:file "other-files")
                 (:file "quicklisp")
                 (:file "egraph-command")
                 (:file "invert"))))
  :in-order-to ((test-op (load-op breeze/test)))
  :perform
  (test-op (o c)
           (uiop:symbol-call
            'breeze.test.main 'run-all-tests)))


;;; breeze/docs system

(defsystem breeze/doc
  :description "Breeze component to generate documentation."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (breeze
               ;; For documentation generation
               spinneret
               closer-mop
               cl-ppcre
               breeze/asdf)
  :pathname "src/"
  :serial nil ; <-
  :components
  ((:file "documentation")
   (:file "report")))


;;; breeze/parachute system

(defsystem breeze/parachute
  :description "Utils for parachute"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (parachute breeze)
  :pathname "src/"
  :components
  ((:file "+parachute")))


;;; Quickproject

(defsystem breeze/quickproject
  :description "Integration with quickproject"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (breeze quickproject)
  :pathname "src/cmds"
  :components
  ((:file "+quickproject")))


;;; breeze.dogfood systeam

(asdf:defsystem #:breeze/dogfood
  :description "Breeze commands and utilities to help with breeze's development."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on ()
  :pathname "src/dogfood/"
  :components
    ((:file "dogfood")))


;;; breeze/test system

(defsystem breeze/test
  :description "Tests for the breeze system."
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (breeze
               parachute
               breeze/parachute
               breeze/doc
               breeze/quickproject
               breeze/dogfood)
  :pathname "tests"
  :components
  ((:file "analysis" :depends-on ("pattern"))
   (:file "command")
   (:file "report")
   (:file "documentation" :depends-on ("xref" "report"))
   (:file "dummy-package")
   (:file "egraph")
   (:file "iterator")
   (:file "lint")
   (:file "listener" :depends-on ("command"))
   (:file "logging")
   ;; TODO move into new module "parser"
   (:file "parser.randomized")
   ;; TODO split out "parser.lisp"
   ;; (:module "parser" ...)
   (:file "parser" :depends-on ("parser.randomized"))
   (:file "parse-tree")
   (:file "package")
   (:file "package-commands")
   (:module "pattern"
    :serial t
    :components ((:file "pattern")
                 (:file "compile-pattern")
                 (:file "substitution")
                 (:file "match")
                 (:file "rewrite")))
   (:module "cmds"
    :depends-on ("command")
    :components ((:file "other-files")))
   (:file "refactor")
   (:file "string-utils")
   (:file "utils")
   (:file "buffer")
   (:file "workspace")
   (:file "xref" :depends-on ("dummy-package"))
   (:file "main")))


;; TODO breeze/quickproject/test
