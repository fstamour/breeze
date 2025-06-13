(defpackage #:breeze.test.listener
  (:documentation "Tests for the package breeze.listener")
  (:use #:cl #:breeze.listener)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.listener)

;; TODO eval-last-expression
'asdf
'|asdf|
'|CL|::|IN-PACKAGE|
;; ^^^ Slime doesn't handle this one correctly

#++
breeze.dummy.test:hjkl

#+sbcl
(block nil
  (handler-bind
      ((sb-int:simple-reader-package-error
         (lambda (condition)
           (return condition))))
    (read-from-string
     "breeze.dummy.test:hjkl")))
;; => #<SB-INT:SIMPLE-READER-PACKAGE-ERROR "Symbol ~S not found in the ~A package." {10256B3563}>

#+sbcl
(block nil
    (handler-bind
        ((sb-int:simple-reader-package-error
           (lambda (condition)
             (return condition))))
      (read-from-string
       "breeze.dummy.dum.dum:hjkl")))
;; => #<SB-INT:SIMPLE-READER-PACKAGE-ERROR "Package ~A does not exist." {1025D737E3}>
