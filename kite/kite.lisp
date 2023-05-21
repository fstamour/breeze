(uiop:define-package #:breeze.kite
    (:documentation "")
  (:use #:cl))

(in-package #:breeze.kite)


;; Find empty tests
#++
(loop
  :for test :in (parachute:package-tests '#:breeze.test.reader2)
  :for children = (parachute:children test)
  :for tests = (parachute:tests test)
  :when (and (null children)
             (null tests))
    :collect test)

;; Find tests whose name is a string (I usually use symbols)
#++
(loop
  :for test :in (parachute:package-tests '#:breeze.test.reader2)
  :for name = (parachute:name test)
  :when (stringp name)
    :do (parachute:remove-test test))
