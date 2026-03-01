;;;; System definitions for breeze and auxiliary systems

(in-package #:asdf-user)


;;; breeze systems

(defsystem "breeze/asdf"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "Utilities for asdf"
  :depends-on ("asdf"
               ;; as of 2025-10-31 — alexandria is only used for "flatten"
               "alexandria"
               "uiop")
  :pathname "src/"
  :components ((:file "asdf")))

(defsystem "breeze/generics"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "Generic functions used across breeze's systems."
  :depends-on ("bordeaux-threads")
  :pathname "src/"
  :components ((:file "generics")))

(defsystem "breeze/class-utils"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "Utility macros for defining classes along with common methods."
  :pathname "src/"
  :components ((:file "class-utils")))

(defsystem "breeze/pattern"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "Pattern matching with first-class patterns"
  :depends-on (;; as of 2025-10-31 — only for "flatten"
               "alexandria"
               "breeze/generics"
               "breeze/class-utils")
  :pathname "src/pattern/"
  :serial t
  :components ((:file "iterator")
               (:file "pattern")
               (:file "compile-pattern")
               (:file "substitution")
               (:file "match")
               (:file "rewrite")))

(defsystem "breeze"
  :name "breeze"
  :version "0"
  :maintainer "Francis St-Amour"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :description "A system to help automate work."
  :depends-on (;; Multi-threading
               "bordeaux-threads"
               ;; Utilities
               "alexandria"
               "uiop"
               ;; cl-heap
               "breeze/generics"
               "breeze/class-utils"
               "breeze/pattern")
  :pathname "src/"
  :components
  ((:file "logging")
   (:file "cl")
   (:file "utils")
   (:file "indirection")
   (:file "string-utils" :depends-on ("utils"))
   (:file "queue")
   (:file "channel" :depends-on ("queue"))
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
   (:file "actor"
    :depends-on ("channel"))
   (:file "command"
    :depends-on ("actor"
                 "utils"
                 "configuration"
                 "indirection"
                 ;; for externalp
                 "xref"))
   (:file "thread" :depends-on ("xref"))
   (:file "xref" :depends-on ("utils"))
   (:file "doctor")
   (:file "listener"
    :depends-on ("xref" "command" "cmds/command-utils"))
   (:file "suggestion"
    :depends-on ("listener"))
   (:file "package" :depends-on ("analysis"))
   (:file "lint" :depends-on ("analysis" "command"))
   (:file "cmds/command-utils")
   (:module "cmds"
    :depends-on ("command" "cmds/command-utils"
                           "analysis"
                           "configuration"
                           "utils"
                           "cl"
                           "indirection"
                           "workspace")
    :components ((:file "refactor"
                  :depends-on ("package-commands"))
                 (:file "boilerplate"
                  :depends-on ("refactor"))
                 (:file "quickfix"
                  :depends-on ("refactor"))
                 (:file "quickinsert"
                  :depends-on ("boilerplate"))
                 (:file "breeze-commands"
                  :depends-on ("refactor"))
                 (:file "blueprint")
                 (:file "completion")
                 (:file "editing")
                 (:file "project")
                 (:file "capture")
                 (:file "package-commands")
                 (:file "test-commands")
                 (:file "other-files")
                 (:file "quicklisp")
                 (:file "egraph-commands")
                 (:file "invert")))
   (:file "breeze"
    :depends-on ("configuration" "cmds")))
  :in-order-to ((test-op (load-op breeze/test)))
  :perform
  (test-op (o c)
           (uiop:symbol-call
            'breeze.test.main 'run-tests)))


;;; breeze/cli system

(asdf:defsystem "breeze/cli"
  :description "A command line interface for breeze."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on ("breeze")
  :pathname "src"
  :components
  ((:file "cli"))
  ;; in order to test this system, load the test system
  ;; :in-order-to ((test-op (load-op breeze/cli/test)))
  ;; this tells asdf what to execute to run the tests
  ;; :perform
  #++ (test-op (o c)
           (uiop:symbol-call
            'breeze/cli.test 'run-tests))
  :build-operation "program-op"
  :build-pathname "../build/brz"
  :entry-point "breeze.cli:main")

(asdf:defsystem #:breeze/cli/test
  :description ""
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause Licence"
  :depends-on ()
  :pathname "tests"
  :components
    ((:file "cli")))


;;; breeze/docs system

(defsystem "breeze/doc"
  :description "Breeze component to generate documentation."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on ("breeze"
               "closer-mop"
               "cl-ppcre"
               "breeze/asdf"
               (:feature :sbcl "sb-introspect"))
  :pathname "src/"
  :serial nil ; <-
  :components
  ((:file "html")
   (:file "documentation" :depends-on ("html"))
   (:file "report" :depends-on ("html"))))


;;; breeze/parachute system

(defsystem "breeze/parachute"
  :description "Utils for parachute"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on ("parachute" "breeze")
  :pathname "src/"
  :components
  ((:file "+parachute")))


;;; breeze/quickproject system

(defsystem "breeze/quickproject"
  :description "Integration with quickproject"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on ("breeze" "quickproject")
  :pathname "src/cmds"
  :components
  ((:file "+quickproject")))

;; TODO breeze/quickproject/test


;;; breeze.dogfood system

(asdf:defsystem "breeze/dogfood"
  :description "Breeze commands and utilities to help with breeze's development."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on ()
  :pathname "src/dogfood/"
  :components
    ((:file "dogfood")))


;;; breeze/test system

(defsystem "breeze/test"
  :description "Tests for the breeze system."
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on ("breeze"
               "parachute"
               "breeze/parachute"
               "breeze/doc"
               "breeze/quickproject"
               "breeze/dogfood")
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
   (:module "parser"
    :components
    ((:file "parser.randomized")
     ;; TODO split out "parser.lisp"
     (:file "parser" :depends-on ("parser.randomized"))
     (:file "parse-tree" :depends-on ("parser.randomized"))
     (:file "incremental-parser")))
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
    :components ((:file "refactor")
                 (:file "other-files")))
   (:file "string-utils")
   (:file "utils")
   (:file "buffer")
   (:file "cl")
   (:file "workspace")
   (:file "xref" :depends-on ("dummy-package"))
   (:file "main")))
