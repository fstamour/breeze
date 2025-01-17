(in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.xref
    (:documentation "Tests for breeze.xref.")
  (:mix #:breeze.xref #:cl #:alexandria)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false))

(in-package #:breeze.test.xref)

(define-test find-package
  (is equal
      (find-packages-by-prefix "breeze")
      (find-packages-by-prefix "breeze.")))

(defparameter *symbols*
  '(dum:*bound-variable*
    dum:*unbound-variable*
    dum:a-class
    dum:a-function
    dum:a-macro
    dum:slot
    (setf dum:slot)))

(define-test predicate-dont-signal-any-error
  (do-external-symbols (symbol 'breeze.dummy.test)
    (mapcar #'(lambda (predicate)
                (funcall predicate symbol))
            '(generic-method-p
              classp
              specialp
              macrop
              simple-function-p))))

(define-test generic-method-p
  (false (generic-method-p 'dum:*bound-variable*))
  (false (generic-method-p 'dum:*unbound-variable*))
  (false (generic-method-p 'dum:a-class))
  (false (generic-method-p 'dum:a-function))
  (false (generic-method-p 'dum:a-macro))
  (true (generic-method-p 'dum:slot))
  (true (generic-method-p '(setf dum:slot))))

(define-test specialp
  (true (specialp 'dum:*bound-variable*))
  (true (specialp 'dum:*unbound-variable*))
  (false (specialp 'dum:a-class))
  (false (specialp 'dum:a-function))
  (false (specialp 'dum:a-macro))
  (false (specialp 'dum:slot))
  (false (specialp '(setf dum:slot))))

(define-test macrop
  (false (macrop 'dum:*bound-variable*))
  (false (macrop 'dum:*unbound-variable*))
  (false (macrop 'dum:a-class))
  (false (macrop 'dum:a-function))
  (true (macrop 'dum:a-macro))
  (false (macrop 'dum:slot))
  (false (macrop '(setf dum:slot))))

(define-test simple-function-p
  (false (simple-function-p 'dum:*bound-variable*))
  (false (simple-function-p 'dum:*unbound-variable*))
  (false (simple-function-p 'dum:a-class))
  (true (simple-function-p 'dum:a-function))
  (false (simple-function-p 'dum:a-macro))
  (false (simple-function-p 'dum:slot))
  (false (simple-function-p '(setf dum:slot))))


;; I used this to generate the tests for classp
#+nil
(let* ((fn 'classp)
       (test-cases
         (with-output-to-string (*standard-output*)
           (loop :for symbol :being :the :external-symbol :of 'breeze.dummy.test
                 :for pass = (funcall fn symbol)
                 :unless (search "undocumented" (string-downcase (symbol-name symbol)))
                   :do
                      (format t "~&(is ")
                      (unless pass (format t "(not "))
                      (format t "(~a 'dum:~a)" fn symbol)
                      (unless pass (format t ")"))
                      (format t ")")))))
  (format t "(define-test ~(~a~%~a~))" fn
          (breeze.string:indent-string 2 test-cases)))

(define-test classp
  (false (classp 'dum:*bound-variable*))
  (false (classp 'dum:slot))
  (true (classp 'dum:a-class))
  (false (classp 'dum:a-generic-function))
  (false (classp 'dum:a-macro))
  (false (classp 'dum:another-generic-function))
  (false (classp 'dum:*unbound-variable*))
  (false (classp 'dum:a-function)))
