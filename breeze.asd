;;;; System definitions for breeze and auxiliary systems


;;; breeze.asd package

(defpackage #:breeze.asd
  (:documentation "Package containing breeze's system defintions")
  (:use :cl :asdf))

(in-package #:breeze.asd)


;;; breeze system

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
               )
  :pathname "src"
  :components
  ((:file "logging")
   (:file "cl")
   (:file "utils")
   (:file "indirection")
   (:file "string-utils" :depends-on ("utils"))
   (:file "generics")
   (:file "channel")
   ;; "test-file" is for parsing ERT files
   (:file "test-file" :depends-on ("utils" "string-utils"))
   (:file "configuration")
   (:file "iterator" :depends-on ("generics"))
   (:file "parser"
    :depends-on ("utils" "generics" "iterator"))
   (:file "incremental-parser"
    :depends-on ("parser" "workspace"))
   (:file "buffer" :depends-on ("parser" "package"))
   (:file "workspace" :depends-on ("parser" "buffer"))
   (:file "pattern" :depends-on ("iterator" "generics"))
   (:file "egraph")
   (:file "analysis" :depends-on ("parser" "pattern"))
   (:file "command"
    :depends-on ("utils"
                 "configuration"
                 "indirection"))
   (:file "asdf")
   (:file "thread" :depends-on ("xref"))
   (:file "xref" :depends-on ("utils"))
   (:file "doctor")
   (:file "listener"
    :depends-on ("xref" "command"))
   (:file "suggestion"
    :depends-on ("listener"))
   (:file "editing" :depends-on ("analysis" "command"))
   (:file "package" :depends-on ("analysis"))
   (:file "package-commands" :depends-on ("analysis" "command"))
   (:file "lint" :depends-on ("analysis" "command"))
   (:file "refactor" :depends-on ( "command" "cl" "analysis"
                                   "utils" "indirection"
                                   "workspace"))
   (:file "completion"
          :depends-on ("command" "analysis"))
   (:file "project" :depends-on ("utils" "command" "configuration"))
   (:file "capture" :depends-on ("utils" "command" "configuration")))
  :in-order-to ((test-op (load-op breeze/test)))
  :perform
  (test-op (o c)
           (uiop:symbol-call
            'breeze.test.main 'run-breeze-tests)))


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
               cl-ppcre)
  :pathname "src"
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
  :pathname "src"
  :components
  ((:file "+parachute")))


;;; Quickproject

(defsystem breeze/quickproject
  :description "Integration with quickproject"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (breeze quickproject)
  :pathname "src"
  :components
  ((:file "+quickproject")))


;;; breeze/test system

(defsystem breeze/test
  :description "Tests for the breeze system."
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (breeze parachute breeze/parachute breeze/doc breeze/quickproject)
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
   (:file "parser" :depends-on ("parser.randomized"))
   (:file "parser.randomized")
   (:file "package")
   (:file "package-commands")
   (:file "pattern")
   (:file "refactor")
   (:file "string-utils")
   (:file "utils")
   (:file "buffer")
   (:file "workspace")
   (:file "xref" :depends-on ("dummy-package"))
   (:file "main")))


;; TODO breeze/quickproject/test
