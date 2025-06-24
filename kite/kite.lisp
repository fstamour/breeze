
;;; breeze.kite package

(uiop:define-package #:breeze.kite
    (:documentation "Utilities for the test framework parachute.")
  (:use #:cl)
  (:export
   #:is
   #:is-equalp
   #:is-equalp*))

(in-package #:breeze.kite)


;;; WIP

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


;;; Assertion helpers

(defun is (&key
             comparator
             expected
             got
             form
             description format-args)
  "The defun equivalent of the parachute:is macro."
  (parachute:eval-in-context
   parachute:*context*
   (make-instance 'parachute:comparison-result
                  :expression `(is ,comparator ,expected ,form)
                  :value-form form
                  :body got
                  :expected expected
                  :comparison comparator
                  :description (when description
                                 (apply 'format nil description format-args)))))


(defun is-equalp (&key
                    (comparator 'equalp)
                    input
                    got
                    (form nil form-supplied-p)
                    expected
                    description
                    format-args)
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
    (flet ((fmt (&rest args)
             (let ((str (apply #'format nil args)))
               (unless parachute:*context*
                 (format *trace-output* "~&~a" str))
               str)))
      (is
       :comparator comparator
       :expected expected
       :got got
       :form (if form-supplied-p form got)
       :description control
       :format-args (list input
                          (list description format-args) got expected))
      (if (equalp expected got)
          (values got :passed)
          (values
           got
           (fmt control
                input (list description format-args) got expected))))))


(defun is-equalp* (input got &optional expected
                               (comparator 'equalp)
                               description &rest format-args)
  "Helper for testing that GOT and EXPECTED are EQUALP.

Can be run interactively.

Can be run without an expected value.

Will always return GOT.

If GOT is not equalp to EXPECTED, generate a nice error message. Print
that message to *trace-output* and return it as a second value.
"
  (is-equalp
   :comparator comparator
   :input input
   :got got
   :expected expected
   :description description
   :format-args format-args))


#|
Examples

(is-equalp* "32 " 2)
returns 2
prints:
For «32 »
got:
2
expected:
NIL

(is-equalp* "32 " 2 1)
returns 2
prints
For «32 »
got:
2
expected:
1

(is-equalp* "32 " 2 2)
returns 2
doesn't print

(is-equalp* "32 " 2 1 'equalp "  (~{~a~^, ~})" '(a b c))
returns 2
prints
For «32 »  (A, B, C)
got:
2
expected:
1

;; (is-equalp* "32 " 2 1 'equalp "  (~a ~s)" "thirty-two" 32)

|#
