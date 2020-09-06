(in-package #:common-lisp-user)

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

(defparameter *symbols*
  '(dum:*bound-variable*
    dum:*unbound-variable*
    dum:a-class
    dum:a-function
    dum:a-macro
    dum:slot
    (setf dum:slot)))

(deftest predicate-dont-signal-any-error
  (do-external-symbols (symbol 'breeze.dummy.test)
    (mapcar #'(lambda (predicate)
		(funcall predicate symbol))
	    '(generic-method-p
	      classp
	      specialp
	      macrop
	      simple-function-p))))

(deftest generic-method-p
  (is (not (generic-method-p 'dum:*bound-variable*)))
  (is (not (generic-method-p 'dum:*unbound-variable*)))
  (is (not (generic-method-p 'dum:a-class)))
  (is (not (generic-method-p 'dum:a-function)))
  (is (not (generic-method-p 'dum:a-macro)))
  (is (generic-method-p 'dum:slot))
  (is (generic-method-p '(setf dum:slot))))

(deftest specialp
  (is (specialp 'dum:*bound-variable*))
  (is (specialp 'dum:*unbound-variable*))
  (is (not (specialp 'dum:a-class)))
  (is (not (specialp 'dum:a-function)))
  (is (not (specialp 'dum:a-macro)))
  (is (not (specialp 'dum:slot)))
  (is (not (specialp '(setf dum:slot)))))

(deftest macrop
  (is (not (macrop 'dum:*bound-variable*)))
  (is (not (macrop 'dum:*unbound-variable*)))
  (is (not (macrop 'dum:a-class)))
  (is (not (macrop 'dum:a-function)))
  (is (macrop 'dum:a-macro))
  (is (not (macrop 'dum:slot)))
  (is (not (macrop '(setf dum:slot)))))

(deftest simple-function-p
  (is (not (simple-function-p 'dum:*bound-variable*)))
  (is (not (simple-function-p 'dum:*unbound-variable*)))
  (is (not (simple-function-p 'dum:a-class)))
  (is (simple-function-p 'dum:a-function))
  (is (not (simple-function-p 'dum:a-macro)))
  (is (not (simple-function-p 'dum:slot)))
  (is (not (simple-function-p '(setf dum:slot)))))


;; I used this to generate the tests fo classp
#+nil
(let* ((fn 'classp)
       (test-cases
	(with-output-to-string (*standard-output*)
	  (loop :for symbol :being :the :external-symbol :of 'breeze.dummy.test
	     :for pass = (funcall fn symbol)
	     :unless (str:containsp "undocumented" (string-downcase (symbol-name symbol)))
	     :do
	       (format t "~&(is ")
	       (unless pass (format t "(not "))
	       (format t "(~a 'dum:~a)" fn symbol)
	       (unless pass (format t ")"))
	       (format t ")")))))
  (format t "(deftest ~(~a~%~a~))" fn
	  (breeze.utils:indent-string 2 test-cases)))

(deftest classp
  (is (not (classp 'dum:*bound-variable*)))
  (is (not (classp 'dum:slot)))
  (is (classp 'dum:a-class))
  (is (not (classp 'dum:a-generic-function)))
  (is (not (classp 'dum:a-macro)))
  (is (not (classp 'dum:another-generic-function)))
  (is (not (classp 'dum:*unbound-variable*)))
  (is (not (classp 'dum:a-function))))
