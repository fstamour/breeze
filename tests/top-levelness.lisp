(defpackage #:breeze.test.top-levelness
  (:documentation "Test package for #:breeze.top-levelness")
  (:use #:cl #:breeze.top-levelness #:breeze.parser)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:true
                #:false))

(in-package #:breeze.test.top-levelness)

(defun test-toplevel-forms (string)
  (let* ((parse-state (parse string))
         ($body (top-level-forms parse-state)))
    (when $body
      (values $body (unless (donep $body) (value $body))))))

(define-test+run toplevel-forms
  (breeze+parachute:pass-p ('progn)
    (false (test-toplevel-forms "(progn)")
           "Should not recurse into empty progn.")
    (false (test-toplevel-forms "(progn #|nothing|#)")
           "Should not recurse into progn with only ignorable nodes.")
    (is eqv
        (iterator-value (token 7 16 :name "SOMETHING"))
        (test-toplevel-forms "(progn something)")
        "Should recurse into progn.")
    (is eqv
        (iterator-value (token 7 16 :name "SOMETHING"))
        (test-toplevel-forms "(progn something")
        "Works on broken code too!"))
  (breeze+parachute:pass-p ('eval-when)
    (false (test-toplevel-forms "(eval-when)")
           "Should not recurse into empty eval-when.")
    (false (test-toplevel-forms "(eval-when #|nothing|#)")
           "Should not recurse into eval-when with only ignorable nodes.")
    (false (test-toplevel-forms "(eval-when (situation) #|nothing|#)")
           "Should not recurse into eval-when with only \"situations\" and ignorable nodes.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-2"))
        (test-toplevel-forms "(eval-when :sunny something-2)")
        "Should recurse into eval-when.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-3"))
        (test-toplevel-forms "(eval-when (:rainy) something-3)")
        "Should recurse into eval-when.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-4"))
        (test-toplevel-forms "(eval-when (:cloudy) something-4")
        "Works on broken code too!")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-5"))
        (test-toplevel-forms "(eval-when (:modly) something-5 else-1")
        "Works on broken code too!"))
  (breeze+parachute:pass-p ('macrolet)
    (false (test-toplevel-forms "(macrolet)")
           "Should not recurse into empty macrolet.")
    (false (test-toplevel-forms "(macrolet #|nothing|#)")
           "Should not recurse into macrolet with only ignorable nodes.")
    (false (test-toplevel-forms "(macrolet (situation) #|nothing|#)")
           "Should not recurse into macrolet with only \"situations\" and ignorable nodes.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-6"))
        (test-toplevel-forms "(macrolet :sunny something-6)")
        "Should recurse into macrolet.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-7"))
        (test-toplevel-forms "(macrolet (:rainy) something-7)")
        "Should recurse into macrolet.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-8"))
        (test-toplevel-forms "(macrolet (:cloudy) something-8")
        "Works on broken code too!")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-9"))
        (test-toplevel-forms "(macrolet (:modly) something-9 else-2")
        "Works on broken code too!"))
  (breeze+parachute:pass-p ('symbol-macrolet)
    (false (test-toplevel-forms "(symbol-macrolet)")
           "Should not recurse into empty symbol-macrolet.")
    (false (test-toplevel-forms "(symbol-macrolet #|nothing|#)")
           "Should not recurse into symbol-macrolet with only ignorable nodes.")
    (false (test-toplevel-forms "(symbol-macrolet (situation) #|nothing|#)")
           "Should not recurse into symbol-macrolet with only \"situations\" and ignorable nodes.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-10"))
        (test-toplevel-forms "(symbol-macrolet :sunny something-10)")
        "Should recurse into symbol-macrolet.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-11"))
        (test-toplevel-forms "(symbol-macrolet (:rainy) something-11)")
        "Should recurse into symbol-macrolet.")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-12"))
        (test-toplevel-forms "(symbol-macrolet (:cloudy) something-12")
        "Works on broken code too!")
    (is eqv
        (iterator-value (token :_ :_ :name "SOMETHING-13"))
        (test-toplevel-forms "(symbol-macrolet (:modly) something-13 else-3")
        "Works on broken code too!"))
  (breeze+parachute:pass-p ('declarations-in-macrolet)
    (is eqv
        (iterator-value (token :_ :_ :name "BODY-1"))
        (test-toplevel-forms "(macrolet ((m () nil)) (declare (optimize speed)) body-1)")
        "Should skip a (declare ...) form between macrolet definitions and body.")
    (is eqv
        (iterator-value (token :_ :_ :name "BODY-2"))
        (test-toplevel-forms
         "(symbol-macrolet ((x 1)) (declare (optimize speed)) (declare (debug 3)) body-2)")
        "Should skip multiple (declare ...) forms in symbol-macrolet."))
  (breeze+parachute:pass-p ('locally)
    (false (test-toplevel-forms "(locally)")
           "Should not recurse into empty locally.")
    (false (test-toplevel-forms "(locally #|nothing|#)")
           "Should not recurse into locally with only ignorable nodes.")
    (is eqv
        (iterator-value (token :_ :_ :name "BODY-3"))
        (test-toplevel-forms "(locally body-3)")
        "Should recurse into locally with no declarations.")
    (is eqv
        (iterator-value (token :_ :_ :name "BODY-4"))
        (test-toplevel-forms "(locally (declare (optimize speed)) body-4)")
        "Should skip a leading (declare ...) form in locally.")
    (is eqv
        (iterator-value (token :_ :_ :name "BODY-5"))
        (test-toplevel-forms
         "(locally (declare (optimize speed)) (declare (debug 3)) body-5)")
        "Should skip multiple leading (declare ...) forms in locally.")
    (is eqv
        (iterator-value (token :_ :_ :name "BODY-6"))
        (test-toplevel-forms "(locally (declare (optimize speed)) body-6")
        "Works on broken code too!")))


;; (trace breeze.pattern:match :wherein test-toplevel-forms)
