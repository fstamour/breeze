
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

(deftest generic-method-p)

(deftest specialp
  (is (specialp 'dum:*bound-variable-documented*))
  (is (specialp 'dum:*unbound-variable-documented*))
  (is (specialp 'dum:*bound-variable-undocumented*))
  (is (specialp 'dum:*unbound-variable-undocumented*))
  (is (not (specialp 'dum:function-documented))))
