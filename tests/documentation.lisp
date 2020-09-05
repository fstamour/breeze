(in-package #:common-lisp-user)

(defpackage #:breeze.documentation.test
  (:use :cl #:breeze.documentation)
  (:import-from #:breeze.test
		#:deftest
		#:is))

(in-package #:breeze.documentation.test)

(find-undocumented-symbols 'breeze.dummy.test)

;; Notes:
;;
;; Are undetected:
;; - *unbound-variable-undocumented*
;; - generic-function-undocumented 
 
