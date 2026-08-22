(defpackage #:breeze.lambda-checks
  (:documentation "Linter checks related to lambda forms")
  (:use #:cl
        #:breeze.analysis
        #:breeze.checks
        #:breeze.diagnostics))

(in-package #:breeze.lambda-checks)

#|

TODO #'(lambda () ...) => (lambda ())

|#

;; (breze.parser:tree (breeze.parser:parse "#.(lambda)"))
