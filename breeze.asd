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
               chanl
               ;; To create projects (scaffolds)
               quickproject
               ;; Utilities
               alexandria
               uiop)
  :pathname "src"
  :components
  ((:file "logging")
   (:file "cl")
   (:file "utils")
   (:file "indirection")
   (:file "string-utils" :depends-on ("utils"))
   (:file "test-file" :depends-on ("utils" "string-utils"))
   (:file "configuration")
   (:file "iterator")
   (:file "lossless-reader" :depends-on ("utils" "iterator"))
   (:file "pattern")
   (:file "egraph")
   (:file "analysis" :depends-on ("lossless-reader" "pattern"))
   (:file "command"
    :depends-on ("utils"
                 "configuration"
                 "indirection"))
   (:file "asdf")
   (:file "thread" :depends-on ("xref"))
   (:file "xref" :depends-on ("utils"))
   (:file "doctor")
   (:file "listener"
    :depends-on ("xref"
                 "command"))
   (:file "suggestion"
    :depends-on ("listener"))
   (:file "refactor" :depends-on ( "command" "cl" "analysis"
                                   "utils" "indirection"))
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


;;; breeze/kite system

(defsystem breeze/kite
  :description "A breeze in a parachute makes a kite: utils for parachute"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (parachute breeze)
  :pathname "kite"
  :serial t
  :components
  ((:file "kite")))


;;; breeze/test system

(defsystem breeze/test
  :description "Tests for the breeze system."
  :version "0"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (breeze parachute breeze/kite breeze/doc)
  :pathname "tests"
  :serial t
  :components
  ((:file "utils")
   (:file "string-utils")
   (:file "logging")
   (:file "lossless-reader.randomized")
   (:file "iterator")
   (:file "lossless-reader")
   (:file "pattern")
   (:file "analysis")
   (:file "egraph")
   (:file "command")
   (:file "refactor")
   (:file "dummy-package")
   (:file "xref")
   (:file "documentation")
   (:file "main")))
