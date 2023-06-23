(defpackage #:breeze.test.pattern
  (:documentation "Test package for breeze.pattern.")
  (:use #:cl #:breeze.pattern)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:true
                #:false
                #:of-type)
  (:import-from #:breeze.pattern
                ;; Structures
                #:ref
                #:ref-name
                #:refp
                #:ref=
                #:term
                #:termp
                #:term-name
                #:term=
                #:typed-term
                #:typed-term-p
                #:typed-term-name
                #:typed-term-type
                #:typed-term=
                #:maybe
                #:maybep
                #:maybe-pattern
                #:maybe=
                #:zero-or-more
                #:zero-or-more-p
                #:zero-or-more-pattern
                #:zero-or-more=
                #:alternation
                #:alternationp
                #:alternation-pattern
                #:alternation=
                #:pattern=
                ;; Compilation
                #:term-symbol-p
                #:compile-pattern
                #:ref-pattern
                #:defpattern
                ;; Iterator
                #:make-iterator
                #:iterator-vector
                #:iterator-position
                #:iterator-step
                #:iterator-parent
                #:iterator-done-p
                #:iterator-push
                #:iterator-maybe-push
                #:iterator-maybe-pop
                #:iterate
                #:iterator-next
                #:iterator-value
                ;; Match
                #:match))

(in-package #:breeze.test.pattern)

(define-test ref
  (let ((ref (ref :x)))
    (of-type ref ref)
    (true (refp ref))
    (is eq :x (ref-name ref))))

(define-test ref=
  (true (ref= (ref :x) (ref :x)))
  (false (ref= (ref :y) (ref :x)))
  (false (ref= (ref :y) 42)))

(define-test term
  (let ((term (term :x)))
    (of-type term term)
    (true (termp term))
    (is eq :x (term-name term))))

(define-test term=
  (true (term= (term :x) (term :x)))
  (false (term= (term :y) (term :x)))
  (false (term= (term :y) 42)))

(define-test typed-term
  (let ((typed-term (typed-term 'symbol :x)))
    (of-type typed-term typed-term)
    (true (typed-term-p typed-term))
    (is eq :x (typed-term-name typed-term))
    (is eq 'symbol (typed-term-type typed-term))))

(define-test typed-term=
  (is typed-term= (term :x) (term :x))
  (isnt typed-term= (term :x) (term :y))
  (let ((expected (typed-term 'symbol :x)))
    (is typed-term= expected (typed-term 'symbol :x))
    (isnt typed-term= expected (typed-term 'symbol :y))
    (isnt typed-term= expected (typed-term 'keyword :x))
    (isnt typed-term= expected (typed-term 'keyword :y))))

(define-test maybe
  (let ((maybe (maybe :x)))
    (of-type maybe maybe)
    (true (maybep maybe))
    (is eq :x (maybe-pattern maybe))))

;; TODO maybe=

(define-test zero-or-more
  (let ((zero-or-more (zero-or-more :x)))
    (of-type zero-or-more zero-or-more)
    (true (zero-or-more-p zero-or-more))
    (is eq :x (zero-or-more-pattern zero-or-more))))

;; TODO zero-or-more=

(define-test alternation
  (let ((alternation (alternation :x)))
    (of-type alternation alternation)
    (true (alternationp alternation))
    (is eq :x (alternation-pattern alternation))))

;; TODO alternation=

(define-test pattern=
  (is pattern= 'x 'x)
  (is pattern= '(x) '(x))
  (is pattern= (ref 'x) (ref 'x))
  (is pattern= (term 'x) (term 'x))
  (is pattern= (typed-term 'symbol 'x) (typed-term 'symbol 'x))
  (is pattern= (maybe 'y) (maybe 'y))
  (is pattern= (maybe '(x y)) (maybe '(x y)))
  ;; TODO Maybe I should try to detect this case when compiling...
  (is pattern= (maybe (maybe 'x)) (maybe (maybe 'x)))
  (is pattern= (zero-or-more 'y) (zero-or-more 'y))
  (is pattern= (zero-or-more '(x y)) (zero-or-more '(x y)))
  (is pattern= (zero-or-more (maybe 'x)) (zero-or-more (maybe 'x)))
  (is pattern= (alternation 'y) (alternation 'y))
  (is pattern= (alternation '(x y)) (alternation '(x y)))
  (is pattern= (alternation (maybe 'x)) (alternation (maybe 'x))))



(define-test term-symbol-p
  (true (term-symbol-p :?x))
  (false (term-symbol-p 'x))
  (false (term-symbol-p "?x")))

#++
(define-test ref-symbol-p
  (true (ref-symbol-p :$x))
  (false (ref-symbol-p 'x))
  (false (ref-symbol-p "$x")))

(define-test compile-pattern
  (is pattern= :x (compile-pattern :x))
  (is pattern= 42 (compile-pattern 42))
  (is pattern= (term :?x) (compile-pattern :?x))
  (is pattern= (ref :x) (compile-pattern '(:ref :x)))
  (is pattern= (maybe :x) (compile-pattern '(:maybe :x)))
  (is pattern= (maybe #(:x :y)) (compile-pattern '(:maybe :x :y)))
  (is pattern= (zero-or-more :x) (compile-pattern '(:zero-or-more :x)))
  (is pattern= (zero-or-more #(:x :y)) (compile-pattern '(:zero-or-more :x :y)))
  (is pattern= (alternation :x) (compile-pattern '(:alternation :x)))
  (is pattern= (alternation #(:x :y)) (compile-pattern '(:alternation :x :y))))



(defpattern a
    a ?a)

(defpattern b
    (:ref a) (:ref a))


;;; Pattern iterators...

;;; Imagine I have a structure that looks like this:

;; `(defun <ws> foo <ws> (x <ws> y) <nl> <ws> (+ <ws> x <ws> y))
;; where <ws> stands for whitespaces and <nl> for newlines

;; To match it against

;; `(defun ?name ?ordinary-lambda-list ?body)

;; I _cannot_ iterate over both in at the same speed.

(define-test make-iterator
  (let* ((vector #(1 2 3))
         (iterator (make-iterator :vector vector)))
    (is eq vector (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator))))

(define-test iterator-done-p
  (true (iterator-done-p (make-iterator :vector #())))
  (true (iterator-done-p (make-iterator :vector #() :position -1)))
  (true (iterator-done-p (make-iterator :vector #() :position 1)))
  (false (iterator-done-p (make-iterator :vector #(1))))
  (false (iterator-done-p (make-iterator :vector #(1 2 3))))
  (true (iterator-done-p (make-iterator :vector #(1 2 3) :position 10))))

#++
(define-test iterator-push
  (let* ((vector1 #(1 2 3))
         (vector2 #(a b c d e f))
         (iterator (iterator-push
                    (make-iterator :vector vector1)
                    vector2)))
    (is eq vector2 (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator))))

#++
(define-test iterator-maybe-push
  ;; empty case, so the iterator is donep from the start
  (let* ((vector #())
         (iterator (iterator-maybe-push (make-iterator :vector vector))))
    (is eq vector (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator)))
  ;; non-empty, no ref
  (let* ((vector #(1 2 3))
         (iterator (iterator-maybe-push (make-iterator :vector vector))))
    (is eq vector (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator)))
  ;; starts with a ref
  (let* ((ref (ref 'a))
         (vector `#(,ref))
         (root-iterator (make-iterator :vector vector))
         (iterator (iterator-maybe-push root-iterator)))
    (isnt eq root-iterator iterator)
    (is eq (ref-pattern ref) (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator))
    (is pattern= 'a (iterator-value iterator))))

#++
(define-test iterator-next
  ;; empty case, so the iterator is donep from the start
  (let* ((vector #())
         (iterator (iterator-next (iterator-maybe-push (make-iterator :vector vector)))))
    ;; TODO check done-p
    ;; TODO check value
    (is eq vector (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator)))
  ;; non-empty, no ref
  (let* ((vector #(1 2 3))
         (iterator (iterator-next (iterator-maybe-push (make-iterator :vector vector)))))
    ;; TODO
    (is eq vector (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator)))
  ;; starts with a ref
  (let* ((ref (ref 'a))
         (vector `#(,ref))
         (root-iterator (make-iterator :vector vector))
         (iterator (iterator-next (iterator-maybe-push root-iterator))))
    ;; TODO
    (isnt eq root-iterator iterator)
    (is eq (ref-pattern ref) (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator))
    (is pattern= 'a (iterator-value iterator))))

#++
(define-test iterator-maybe-pop
  ;; empty case
  (let* ((vector #())
         (iterator (iterator-maybe-pop (iterator-maybe-push (make-iterator :vector vector)))))
    (is eq vector (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator)))
  ;; non-empty, no ref
  (let* ((vector #(1 2 3))
         (iterator (iterator-maybe-pop (iterator-maybe-push (make-iterator :vector vector)))))
    (is eq vector (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator)))
;;;; WIP
  ;; starts with a ref
  (let* ((ref (ref 'a))
         (vector `#(,ref))
         (root-iterator (make-iterator :vector vector))
         (iterator (iterator-maybe-push root-iterator)))
    (iterator-next)
    (isnt eq root-iterator iterator)
    (is eq (ref-pattern ref) (iterator-vector iterator))
    (is = 0 (iterator-position iterator))
    (is = 1 (iterator-step iterator))
    (is pattern= 'a (iterator-value iterator)))
  )

#++
(defun test-iterator (vector)
  (loop
    :for iterator := (iterate vector) :then (iterator-next iterator)
    :until (iterator-done-p iterator)
    :for value = (iterator-value iterator)
    :do (format *debug-io* "~%~%~S~%~%" value)
    :collect value))
#++
(test-iterator `#(,(ref 'a)))



(defun test-match (pattern input)
  (match (compile-pattern pattern) input))

(define-test "match basic patterns"
  (true (match nil nil))
  (false (match nil t))
  (false (match t nil))
  (true (match t t))
  (true (match 1 1))
  (false (match 1 2))
  (true (match 'x 'x))
  (true (match "x" "x"))
  (false (match 'x 'y))
  (true (match #(a) '(a)))
  ;; TODO add vectors (but not arrays)
  )

(define-test "match terms"
  (true (match (term :?x) nil))
  (true (match (term :?x) 1))
  (true (match (term :?x) 'x))
  (true (match (term :?x) "x"))
  (true (match (term :?x) '(a)))
  (true (match (term :?x) (term :?x)))
  (true (match `#(,(term :?x)) (list 42))))

(define-test "match typed-terms"
  (true (match (typed-term 'null :?x) nil))
  (false (match (typed-term 'null :?x) t))
  (true (match (typed-term 'number :?x) 1))
  (false (match (typed-term 'number :?x) t))
  (true (match (typed-term 'symbol :?x) 'x))
  (false (match (typed-term 'keyword :?x) 'x))
  (true (match (typed-term 'string :?x) "x"))
  (false (match (typed-term 'string :?x) 1))
  (true (match (typed-term 'cons :?x) '(a)))
  (false (match (typed-term 'cons :?x) 'a)))



(defpattern optional-parameters
    &optional
    (:zero-or-more
     (:alternation (:the symbol ?var)
                   ((:the symbol ?var)
                    ?init-form (:maybe (:the symbol ?supplied-p-parameter))))))

(defpattern rest-parameter &rest ?var)

(defpattern body-parameter &body ?var)

(defpattern key-parameters
    &key
    (:zero-or-more
     (:alternation (:the symbol ?var)
                   ((:the symbol ?var)
                    ?init-form (:maybe (:the symbol ?supplied-p-parameter))))))

(defpattern aux-parameters
    &aux
    (:zero-or-more
     (:alternation (:symbol ?var)
                   ((:symbol ?var) ?init-form)))
  (:maybe &allow-other-keys))

(defpattern ordinary-lambda-list
    (:zero-or-more ?var)
    (:ref optional-parameters)
  (:ref rest-parameter)
  (:ref key-parameters)
  (:ref aux-parameters))

(defpattern defun
    (defun (:the symbol ?name) $ordinary-lambda-list ?body))

#++
(match 'optional-parameters
  '(&optional))

#++
(list
 '(&optional x)
 '(&optional (x 1))
 '(&optional (x 1 supplied-p))
 '(&optional x y (z t)))



;; (trace match :methods t)

#++
(define-test "match ref"
  (true (match `#(,(ref 'a)) '(a 42)))
  (true (match `#(,(ref 'b)) '(a 42 a 73)))
  (false (match `#(,(ref 'body-parameter)) '(42)))
  (true (match `#(,(ref 'body-parameter)) '(&body 42))))

;; TODO I tested if match return the right generalized boolean, but I
;; haven't tested the actual value it returns when it's true. Which
;; should be either t or a list of new bindings.
