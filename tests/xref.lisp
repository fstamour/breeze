
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

(deftest generic-method-p
  (is (not (generic-method-p 'dum:*bound-variable-documented*)))
  (is (not (generic-method-p 'dum:*unbound-variable-documented*)))
  (is (not (generic-method-p 'dum:class-documented)))
  (is (not (generic-method-p 'dum:function-documented)))
  (is (not (generic-method-p 'dum:macro-documented)))
  (is (generic-method-p 'dum:slot-documented))
  (is (generic-method-p '(setf dum:slot-documented))))

(deftest specialp
  (is (specialp 'dum:*bound-variable-documented*))
  (is (specialp 'dum:*unbound-variable-documented*))
  (is (not (specialp 'dum:class-documented)))
  (is (not (specialp 'dum:function-documented)))
  (is (not (specialp 'dum:macro-documented)))
  (is (not (specialp 'dum:slot-documented)))
  (is (not (specialp '(setf dum:slot-documented)))))

(deftest macrop
  (is (not (macrop 'dum:*bound-variable-documented*)))
  (is (not (macrop 'dum:*unbound-variable-documented*)))
  (is (not (macrop 'dum:class-documented)))
  (is (not (macrop 'dum:function-documented)))
  (is (macrop 'dum:macro-documented))
  (is (not (macrop 'dum:slot-documented)))
  (is (not (macrop '(setf dum:slot-documented)))))

(deftest simple-function-p
  (is (not (simple-function-p 'dum:*bound-variable-documented*)))
  (is (not (simple-function-p 'dum:*unbound-variable-documented*)))
  (is (not (simple-function-p 'dum:class-documented)))
  (is (simple-function-p 'dum:function-documented))
  (is (not (simple-function-p 'dum:macro-documented)))
  (is (not (simple-function-p 'dum:slot-documented)))
  (is (not (simple-function-p '(setf dum:slot-documented)))))
