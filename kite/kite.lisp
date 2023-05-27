(uiop:define-package #:breeze.kite
    (:documentation "")
  (:use #:cl)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
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


(defun is-equalp (input got &optional (expected nil expectedp))
  "Helper for testing that GOT and EXPECTED are EQUALP.

It is more useful than a plain parachute:is because you can run it
interactively, without an expected value. In which case it will return
the value that was gotten.

It will also print in the *debug-io*.

The main point though, is that the error message is going to be nice.
"
  (let ((*print-pretty* nil)
        (*print-circle* t)
        (*print-right-margin* nil)
        (control "For «~a»~%~tgot:~%~t~t~s~%~texpected:~%~t~t~s"))
    (flet ((fmt (&rest args)
             (let ((str (apply #'format nil args)))
               (unless parachute:*context*
                 (format *debug-io* "~&~a" str))
               str)))
      (if expectedp
          (is equalp expected got control input got expected)
          (true expectedp "«~a» => ~s" input got))
      (unless (equalp expected got)
        (fmt control input got expected))
      got)))

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

|#
