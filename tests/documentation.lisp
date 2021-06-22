(in-package #:common-lisp-user)

(defpackage #:breeze.documentation.test
  (:use :cl #:breeze.documentation)
  (:import-from #:breeze.test
		#:deftest
		#:is))

(in-package #:breeze.documentation.test)

(defun find-undocumented-symbols-in-dummy-package ()
  ""
  (labels ((format-symbol (symbol)
	     (format nil "~a:~a"
		     (package-name (symbol-package symbol))
		     (symbol-name symbol)))
	   (sort-undocumented (undocumented-list)
	     (sort undocumented-list
		   #'(lambda (a b)
		       (destructuring-bind (a-what a-symbol) a
			 (destructuring-bind (b-what b-symbol) b
			   (if (eq a-what b-what)
			       (string< (format-symbol a-symbol)
					(format-symbol b-symbol))
			       (string< (symbol-name a-what)
					(symbol-name b-what)))))))))
    ;; Sort
    (sort-undocumented
     ;; Keep only the first 2 element of each result
     (mapcar #'(lambda (undocumented)
		 (destructuring-bind (what symbol &rest _)
		     undocumented
		   (declare (ignore _))
		   (list what symbol)))
	     ;; Find in Dummy package
	     (find-undocumented-symbols 'breeze.dummy.test)))))

(deftest find-undocumented-symbols
  (let ((undocumented-symbols (find-undocumented-symbols-in-dummy-package)))
    (is
      (equal undocumented-symbols
	     '((:function breeze.dummy.test:function-undocumented)
	       (:generic-method breeze.dummy.test:generic-function-undocumented)
	       (:method breeze.dummy.test:another-generic-function)
	       (:method breeze.dummy.test:generic-function-undocumented)
	       (:package "BREEZE.DUMMY.TEST")
	       (:special-variable breeze.dummy.test:*bound-variable-undocumented*)
	       (:special-variable breeze.dummy.test:*unbound-variable-undocumented*))))))
