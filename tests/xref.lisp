
(uiop:define-package #:breeze.xref.test
    (:documentation "Tests for breeze.xref.")
  (:mix #:breeze.xref #:cl #:alexandria)
  (:import-from #:breeze.test
		#:deftest
		#:is))

(in-package #:breeze.xref.test)

(deftest find-package
  (is (equal
       (find-packages-by-prefix "breeze")
       (find-packages-by-regex "breeze.*"))))
