(defpackage #:breeze.test.html
  (:documentation "Tests for the package breeze.html")
  (:use #:cl #:breeze.html)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish
                #:fail))

(in-package #:breeze.test.html)

(define-test+run escape-html
  (is string= "<br>" (escape-html "<br>"))
  (is string= "&lt;a&gt;" (escape-html "<a>"))
  (is string= "<a href=\"\"></a>" (escape-html "<a href=\"\"></a>"))
  (is string= "=> (#&lt;ASDF/SYSTEM:SYSTEM \"breeze/test\"&gt; #&lt;ASDF/SYSTEM:SYSTEM \"breeze/config\"&gt;)"
      (escape-html "=> (#<ASDF/SYSTEM:SYSTEM \"breeze/test\"> #<ASDF/SYSTEM:SYSTEM \"breeze/config\">)")))
