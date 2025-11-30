(defpackage #:breeze.test.parse-tree
  (:documentation "Test package for #:breeze.parse-tree")
  (:use #:cl #:breeze.parser)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false))

(in-package #:breeze.test.parse-tree)

(defun test-toplevel-preserver-body (string)
  (breeze.parse-tree::top-level-forms
   (breeze.parser:parse string)))

(define-test+run toplevel-preserver-body
  (breeze+parachute:pass-p ('progn)
    (false (test-toplevel-preserver-body "(progn)")
           "Should not recurse into empty progn.")
    (false (test-toplevel-preserver-body "(progn #|nothing|#)")
           "Should not recurse into progn with only ignorable nodes.")
    (is eqv
        (iterator-value (token 7 16 :name "SOMETHING"))
        (test-toplevel-preserver-body "(progn something)")
        "Should recurse into progn.")
    (is eqv
        (iterator-value (token 7 16 :name "SOMETHING"))
        (test-toplevel-preserver-body "(progn something")
        "Works on broken code too!"))
  (breeze+parachute:pass-p ('eval-when)
    (false (test-toplevel-preserver-body "(eval-when)")
           "Should not recurse into empty eval-when.")
    (false (test-toplevel-preserver-body "(eval-when #|nothing|#)")
           "Should not recurse into eval-when with only ignorable nodes.")
    (false (test-toplevel-preserver-body "(eval-when (situation) #|nothing|#)")
           "Should not recurse into eval-when with only \"situations\" and ignorable nodes.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-2"))
        (test-toplevel-preserver-body "(eval-when :sunny something-2)")
        "Should recurse into eval-when.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-3"))
        (test-toplevel-preserver-body "(eval-when (:rainy) something-3)")
        "Should recurse into eval-when.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-4"))
        (test-toplevel-preserver-body "(eval-when (:cloudy) something-4")
        "Works on broken code too!")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-5"))
        (test-toplevel-preserver-body "(eval-when (:modly) something-5 else-1")
        "Works on broken code too!"))
  (breeze+parachute:pass-p ('macrolet)
    (false (test-toplevel-preserver-body "(macrolet)")
           "Should not recurse into empty macrolet.")
    (false (test-toplevel-preserver-body "(macrolet #|nothing|#)")
           "Should not recurse into macrolet with only ignorable nodes.")
    (false (test-toplevel-preserver-body "(macrolet (situation) #|nothing|#)")
           "Should not recurse into macrolet with only \"situations\" and ignorable nodes.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-6"))
        (test-toplevel-preserver-body "(macrolet :sunny something-6)")
        "Should recurse into macrolet.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-7"))
        (test-toplevel-preserver-body "(macrolet (:rainy) something-7)")
        "Should recurse into macrolet.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-8"))
        (test-toplevel-preserver-body "(macrolet (:cloudy) something-8")
        "Works on broken code too!")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-9"))
        (test-toplevel-preserver-body "(macrolet (:modly) something-9 else-2")
        "Works on broken code too!"))
  (breeze+parachute:pass-p ('symbol-macrolet)
    (false (test-toplevel-preserver-body "(symbol-macrolet)")
           "Should not recurse into empty symbol-macrolet.")
    (false (test-toplevel-preserver-body "(symbol-macrolet #|nothing|#)")
           "Should not recurse into symbol-macrolet with only ignorable nodes.")
    (false (test-toplevel-preserver-body "(symbol-macrolet (situation) #|nothing|#)")
           "Should not recurse into symbol-macrolet with only \"situations\" and ignorable nodes.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-10"))
        (test-toplevel-preserver-body "(symbol-macrolet :sunny something-10)")
        "Should recurse into symbol-macrolet.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-11"))
        (test-toplevel-preserver-body "(symbol-macrolet (:rainy) something-11)")
        "Should recurse into symbol-macrolet.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-12"))
        (test-toplevel-preserver-body "(symbol-macrolet (:cloudy) something-12")
        "Works on broken code too!")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-13"))
        (test-toplevel-preserver-body "(symbol-macrolet (:modly) something-13 else-3")
        "Works on broken code too!")))


;; (trace breeze.pattern:match :wherein test-toplevel-preserver-body)
