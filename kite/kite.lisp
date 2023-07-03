(uiop:define-package #:breeze.kite
    (:documentation "Utilities for the test framework parachute.")
  (:use #:cl)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:true
                #:false
                #:of-type)
  (:export #:is-equalp))

(in-package #:breeze.kite)


;; Find empty tests
#++
(loop
  :for test :in (parachute:package-tests '#:breeze.test.lossless-reader)
  :for children = (parachute:children test)
  :for tests = (parachute:tests test)
  :when (and (null children)
             (null tests))
    :collect test)

;; Find tests whose name is a string (I usually use symbols)
#++
(loop
  :for test :in (parachute:package-tests '#:breeze.test.lossless-reader)
  :for name = (parachute:name test)
  :when (stringp name)
    :do (parachute:remove-test test))


(defun is-equalp (input got &optional expected description &rest format-args)
  "Helper for testing that GOT and EXPECTED are EQUALP.

Can be run interactively.

Can be run without an expected value.

Will always return GOT.

If GOT is not equalp to EXPECTED, generate a nice error message. Print
that message to *trace-output* and return it as a second value.
"
  (let ((*print-pretty* nil)
        (*print-circle* t)
        (*print-right-margin* nil)
        ;; Nothing to see here...
        (control
          (concatenate 'string
                       "For "
                       "«~a»"
                       (if description "~1{~?~}" "~*")
                       "~%"
                       "~tgot:~%"
                       "~t~t~s~%"
                       "~texpected:~%"
                       "~t~t~s")))
    #++
    (format t "~%control: ~s~%input: ~s~%got: ~s~%expected: ~s~%description: ~s~%format-args: ~s"
            control input got expected description format-args)
    (flet ((fmt (&rest args)
             (let ((str (apply #'format nil args)))
               (unless parachute:*context*
                 (format *trace-output* "~&~a" str))
               str)))
      (parachute:is equalp expected got
                    control
                    input (list description format-args) got expected)
      (if (equalp expected got)
          got
          (values
           got
           (fmt control
                input (list description format-args) got expected))))))

#|
Examples

(is-equalp "32 " 2)
returns 2
prints:
For «32 »
 got:
  2
 expected:
  NIL

(is-equalp "32 " 2 1)
returns 2
prints
For «32 »
 got:
  2
 expected:
  1

(is-equalp "32 " 2 2)
returns 2
doesn't print

(is-equalp "32 " 2 1 "  (~{~a~^, ~})" '(a b c))
returns 2
prints
For «32 »  (A, B, C)
 got:
  2
 expected:
  1

;; (is-equalp "32 " 2 1 "  (~a ~s)" "thirty-two" 32)

|#
