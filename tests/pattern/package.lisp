(defpackage #:breeze.test.pattern
  (:documentation "Test package for breeze.pattern.")
  (:use #:cl #:breeze.pattern #:breeze.iterator)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:is-values
                #:isnt
                #:true
                #:false
                #:of-type
                #:fail
                #:finish)
  ;; importing non-exported symbols
  (:import-from #:breeze.pattern
                #:symbol-starts-with
                #:var-symbol-p
                #:multi-valued-var-symbol-p
                #:wildcard-symbol-p
                #:bindings
                #:make-substitutions
                #:copy-substitutions
                #:set-binding
                #:add-binding
                #:emptyp))
