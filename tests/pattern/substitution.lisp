;;;; Tests for src/pattern/substitution.lisp

(in-package #:breeze.test.pattern)

(define-test+run make-binding
  (of-type binding
           (make-binding :?x "a")
           "Make binding should return an instance of the class \"binding\".")
  (is equal "#<binding :?x → a>"
      (prin1-to-string
       (make-binding :?x 'a))
      "The specialized print-object method on the class \"binding\" should print the slots \"from\" and \"to\".")
  ;; TODO tests with :pattern and :children
  )

(define-test+run "binding - eqv"
  (true (eqv (make-binding :?x "a") (make-binding :?x "a")))
  (false (eqv (make-binding :?y "a") (make-binding :?z "a")))
  ;; TODO tests with :pattern and :children
  )

(defun bindings-alist (substitutions)
  (etypecase substitutions
      ((eql t) t)
      (null nil)
      (binding
       (list (cons (from substitutions) (to substitutions))))
      (substitutions
       (sort
        (mapcar (lambda (binding)
                  (cons (from binding) (to binding)))
                (alexandria:hash-table-values (bindings substitutions)))
        #'string<
        :key (lambda (binding-cons)
               (symbol-name (car binding-cons)))))))

(define-test+run substitutions
  (of-type substitutions
      (make-substitutions)
      "make-substitutions should return an instance of the class \"substitutions\".")
  (true (emptyp (make-substitutions))
        "make-substitutions should return an empty set of bindings.")
  (is equal
      "#<substitutions (empty)>"
      (prin1-to-string (make-substitutions))
      "The print-object method specialized on the class \"substitutions\" should print it when the binding set is empty.")
  (is equal
      "#<substitutions ((:?x . #<binding :?x → a>))>"
      (prin1-to-string (merge-substitutions (make-substitutions)
                                       (make-binding :?x 'a)))
      "The print-object method specialized on the class \"substitutions\" should print the binding when there's only 1.")
  (is equal
      "#<substitutions (2 bindings)>"
      (prin1-to-string
       (merge-substitutions (make-substitutions)
                       (merge-substitutions (make-binding :?x 'a)
                                       (make-binding :?y 'b))))
      "The print-object method specialized on the class \"substitutions\" should print the number of bindings when there's more than 1."))

(define-test+run "substitutions - eqv"
  (isnt eqv nil (substitutions))
  (is eqv (substitutions) (substitutions))
  (is eqv t (substitutions))
  (is eqv (substitutions) t)
  (is eqv (substitutions `((x 1))) (substitutions `((x 1))))
  (isnt eqv (substitutions `((x 1))) (substitutions))
  (isnt eqv (substitutions) (substitutions `((x 1))))
  (isnt eqv t (substitutions `((x 1))))
  (isnt eqv (substitutions `((x 1))) t)
  (isnt eqv (substitutions `((x 1))) (substitutions `((x 2))))
  (isnt eqv (substitutions `((y 2))) (substitutions `((y 1))))
  (isnt eqv (substitutions `((z 3))) (substitutions `((z 3) (y 4))))
  (is eqv (substitutions `((a 5))) (make-binding 'a 5))
  (is eqv (make-binding 'a 5) (substitutions `((a 5))))
  (isnt eqv (substitutions `((a 5) (b 6))) (make-binding 'a 5))
  (isnt eqv (make-binding 'a 5) (substitutions `((a 5) (b 6))))
  (isnt eqv (make-binding 'a 5) (substitutions))
  (isnt eqv (substitutions) (make-binding 'a 5)))

(define-test+run "substitution - coopy"
  (false
   (let* ((b1 (make-substitutions))
          (b2 (copy-substitutions b1)))
     (eq (bindings b1) (bindings b2)))
   "Copying a bindind-set should create a different hash-table"))

(define-test+run "substitution - find-binding"
  (is-values (find-binding (make-substitutions) :?x)
    (eq nil)
    (eq nil)
    "Should not find a binding in an empty substitutions"))

(define-test+run "substitutions - set-binding and add-binding"
  (is equal '(:?x a)
      (let* ((bs (make-substitutions)))
        (set-binding bs (make-binding :?x 'a))
        (let ((binding (find-binding bs :?x)))
          (list (from binding) (to binding))))
      "Should find the right binding")
  (let* ((bs (make-substitutions)))
    (is eq bs (add-binding bs (make-binding :?x 'a))
        "Adding first bindings should return the substitutions")
    (is eq bs (add-binding bs (make-binding :?y 'b))
        "Adding unrelated bindings should return the substitutions"))
  (let* ((bs (make-substitutions)))
    (add-binding bs (make-binding :?x 'a))
    (is eq bs (add-binding bs (make-binding :?x 'a))
        "Adding the same binding twice should return the substitutions"))
  (let* ((bs (make-substitutions)))
    (add-binding bs (make-binding :?x 'a))
    (false (add-binding bs (make-binding :?x 'b))
           "Adding conflicting bindings should return nil"))
  (is equal '((:?x . a))
      (let* ((bs (make-substitutions)))
        (add-binding bs (make-binding :?x 'a))
        (add-binding bs (make-binding :?x 'b))
        (bindings-alist bs))
      "Adding conflicting bindings should not modify the substitutions"))


(define-test+run merge-substitutions
  ;; merging nil and t
  (progn
    (false (merge-substitutions nil nil))
    (false (merge-substitutions nil t))
    (false (merge-substitutions t nil))
    (true (merge-substitutions t t)))
  ;; merging with nil
  (progn
    (false (merge-substitutions (make-binding :?x 'a) nil)
           "merging with nil should fail")
    (false (merge-substitutions nil (make-binding :?x 'a))
           "merging with nil should fail"))
  ;; merging with t
  (progn
    (true (merge-substitutions (make-binding :?x 'a) t))
    (true (merge-substitutions t (make-binding :?x 'a)))
    (is equal '((:?x . a))
        (bindings-alist (merge-substitutions (make-binding :?x 'a) t)))
    (is equal '((:?x . a))
        (bindings-alist (merge-substitutions t (make-binding :?x 'a)))))
  (is equal '((:?x . a) (:?y . b))
      (bindings-alist (merge-substitutions (make-binding :?x 'a) (make-binding :?y 'b))))
  (finish
   (let ((var 'a)
         (bs (make-substitutions)))
     (add-binding bs (make-binding var 42))
     (is eq bs (merge-substitutions bs (make-binding var 42))))))
