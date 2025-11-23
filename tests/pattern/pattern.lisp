(defpackage #:breeze.test.pattern
  (:documentation "Test package for breeze.pattern.")
  (:use #:cl #:breeze.pattern #:breeze.iterator)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:is-values
                #:isnt
                #:true
                #:false
                #:of-type
                #:fail
                #:finish)
  ;; importing non-exported symbols
  (:import-from #:breeze.pattern
                #:symbol-starts-with
                #:var-symbol-p
                #:multi-valued-var-symbol-p
                #:wildcard-symbol-p
                #:bindings
                #:make-substitutions
                #:copy-substitutions
                #:set-binding
                #:add-binding
                #:emptyp))

(in-package #:breeze.test.pattern)


;;; Wildcard

(define-test+run wildcard
  (let ((wildcard (wildcard)))
    (of-type wildcard wildcard)
    (true (wildcardp wildcard)))
  (is eq (wildcard) (wildcard)
      "The fuction (wildcard) always return the same instance."))

;; Making sure there's a "make-load-form"...
#.(wildcard)


;;; Simple var

(define-test+run simple-var
  (let ((var (svar :x)))
    (of-type simple-var var)
    (true (varp var))
    (is eq :x (name var)))
  (let ((var (svar :y :multi-valued-p nil)))
    (of-type simple-var var)
    (true (varp var))
    (false (multi-valued-p var))
    (is eq :y (name var)))
  (let ((var (svar :z :multi-valued-p t)))
    (of-type simple-var var)
    (true (varp var))
    (true (multi-valued-p var))
    (is eq :z (name var))))

(define-test+run "simple-var - eqv"
  (true (eqv (svar :x) (svar :x)))
  (false (eqv (svar :y) (svar :x)))
  (false (eqv (svar :x :multi-valued-p t) (svar :x :multi-valued-p nil)))
  (false (eqv (svar :x :multi-valued-p t) (svar :x)))
  (true (eqv (svar :x :multi-valued-p nil) (svar :x :multi-valued-p nil)))
  (true (eqv (svar :x :multi-valued-p nil) (svar :x))))

(define-test+run "simple-var - print-object"
  (is string= "(svar :x)" (prin1-to-string (svar :x)))
  ;; TODO maybe (svar 'x) instead
  (is string= "(svar x)" (prin1-to-string (svar 'x)))
  ;; TODO maybe (svar '#:x)
  (is string= "(svar #:x)" (prin1-to-string (svar '#:x)))
  (is string= "(svar :x)"
      (prin1-to-string (svar :x :multi-valued-p nil)))
  (is string= "(svar :x :multi-valued-p t)"
      (prin1-to-string (svar :x :multi-valued-p t)))
  ;; TODO maybe (svar 'x) instead
  (is string= "(svar x :multi-valued-p t)"
      (prin1-to-string (svar 'x :multi-valued-p t)))
  ;; TODO maybe (svar '#:x)
  (is string= "(svar #:x :multi-valued-p t)"
      (prin1-to-string (svar '#:x :multi-valued-p t))))


;;; Var

(define-test+run var
  (let ((var (var :y 42)))
    (of-type var var)
    (true (varp var))
    (is eq :y (name var))
    (is eql 42 (pattern var)))
  (let ((var (var :z nil)))
    (of-type var var)
    (true (varp var))
    (is eq :z (name var))
    (null (pattern var))))

(define-test+run "var - eqv"
  (progn
    (true (eqv (var :x nil) (var :x nil)))
    (false (eqv (var :y nil) (var :x nil)))
    (true (eqv (var :x 31) (var :x 31)))
    (false (eqv (var :z 31) (var :x 31))))
  (progn
    (true (eqv (var :x nil :multi-valued-p t) (var :x nil :multi-valued-p t)))
    (false (eqv (var :y nil :multi-valued-p t) (var :x nil :multi-valued-p t)))
    (true (eqv (var :x 31 :multi-valued-p t) (var :x 31 :multi-valued-p t)))
    (false (eqv (var :z 31 :multi-valued-p t) (var :x 31 :multi-valued-p t)))
    (false (eqv (var :y nil :multi-valued-p t) 42)))
  (progn
    (true (eqv (var :x nil :multi-valued-p nil) (var :x nil :multi-valued-p nil)))
    (false (eqv (var :y nil :multi-valued-p nil) (var :x nil :multi-valued-p nil)))
    (true (eqv (var :x 31 :multi-valued-p nil) (var :x 31 :multi-valued-p nil)))
    (false (eqv (var :z 31 :multi-valued-p nil) (var :x 31 :multi-valued-p nil)))
    (false (eqv (var :y nil :multi-valued-p nil) 42)))
  (progn
    (false (eqv (var :x nil :multi-valued-p t) (var :x nil :multi-valued-p nil)))
    (false (eqv (var :y nil :multi-valued-p t) (var :x nil :multi-valued-p nil)))
    (false (eqv (var :x 31 :multi-valued-p t) (var :x 31 :multi-valued-p nil)))
    (false (eqv (var :z 31 :multi-valued-p t) (var :x 31 :multi-valued-p nil)))))

(define-test+run "var - print-object"
  (is string= "(var :x nil)" (prin1-to-string (var :x nil)))
  ;; TODO would be nice if it printed as "(var :z 'a)"
  (is string= "(var :z a)" (prin1-to-string (var :z 'a)))
  (is string= "(var :za 32)" (prin1-to-string (var :za 32))))

(define-test+run "sym - print-object"
  (is string= "(sym 'cl :wild)"
      (format nil "~a" (sym 'cl :wild)))
  (is string= "(sym 'cl 'defun)"
      (format nil "~a" (sym 'cl 'defun)))
  (is string= "(sym 'cl '#:defun :possibly-internal-symbol)"
      (format nil "~a" (sym 'cl '#:defun :possibly-internal-symbol)))
  (is string=
      "(sym (find-package '#:UIOP/PACKAGE) \"DEFINE-PACKAGE\")"
      (let ((symbol 'uiop:define-package))
        (princ-to-string (sym (symbol-package symbol)
                              (symbol-name symbol))))))


;;; Repetitions

(define-test+run repetition
  (let ((p (repetition 42 :min 0 :max 100)))
    (of-type repetition p)
    (is eqv 42 (pattern p))
    (is = 0 (minimum p))
    (is = 100 (maximum p))
    (false (name p))))

(define-test+run "repetition - eqv"
  (true (eqv (repetition 42) (repetition 42)))
  (false (eqv (repetition 'x) (repetition 42)))
  (true (eqv (repetition 'x) (repetition 'x)))
  (true (eqv (repetition 42 :min 0 :max 100) (repetition 42 :min 0 :max 100))))

;; TODO repetition - print-object


(define-test+run maybe
  (let ((maybe (maybe :x)))
    (of-type repetition maybe)
    (is eq :x (pattern maybe))
    (is = 0 (minimum maybe)
        "A pattern created with the function `maybe' should have a minimum of 0.")
    (is = 1 (maximum maybe)
        "A pattern created with the function `maybe' should have a maximum of 1.")))

(define-test+run "maybe - eqv"
  (is eqv (maybe 'y) (maybe 'y))
  (is eqv (maybe '(x y)) (maybe '(x y)))
  ;; TODO Maybe I should try to detect this case when compiling...
  (is eqv (maybe (maybe 'x)) (maybe (maybe 'x))))

;; TODO "maybe - print-object"

(define-test+run zero-or-more
  (let ((zero-or-more (zero-or-more :x)))
    (of-type repetition zero-or-more)
    (is eq :x (pattern zero-or-more))
    (is = 0 (minimum zero-or-more)
        "A pattern created with the function `zero-or-more' should have a minimum of 0.")
    (false (maximum zero-or-more)
           "A pattern created with the function `zero-or-more' should not have a maximum.")))

(define-test+run "zero-or-more - eqv"
  (is eqv (zero-or-more 'y) (zero-or-more 'y))
  (is eqv (zero-or-more '(x y)) (zero-or-more '(x y))))

;; TODO "zero-or-more - print-object"

;; TODO one-or-more

;; TODO "one-or-more - print-object"


;;; Either

(define-test+run either
  (let ((either (either #(:x))))
    (of-type either either)
    (true (eitherp either))
    (is equalp #(:x) (patterns either))))

(define-test+run "either - eqv"
  (is eqv (either #(y)) (either #(y)))
  (is eqv (either #(x y)) (either #(x y)))
  (is eqv
      (either (vector (maybe 'x)))
      (either (vector (maybe 'x)))))

;; TODO "either - print-object"


;;; More complex comparison of patterns

(define-test+run eqv
  (is eqv 'x 'x)
  (is eqv '(x) '(x))
  (is eqv '#(x) '#(x))
  (is eqv '(#(x)) '(#(x)))
  (is eqv (zero-or-more (maybe 'x)) (zero-or-more (maybe 'x))))


;;; Pattern iterators...

(define-test+run pattern-iterator
  (false (collect (make-pattern-iterator #())))
  (is equalp '(a) (collect (make-pattern-iterator #(a)))))


;;; Parsing cl:loop using patterns

#++ ;; sample "loop" forms
(let ((samples `((loop for el in list do (print el))
                 (loop :repeat 10 #:collect 'bob)
                 (loop :for (head . tail) :on lst :while tail append tail)
                 (loop :for c across string thereis (char= c #\+))
                 (loop :for (k v) :on plist :by #'cddr
                       :sum v :into vsum
                       :max k :into kmax
                       :finally (return (values kmax vsum)))
                 (loop)))))


;; TODO it might be easier to debug if each patterns are compiled separatedly
#++
(let* ((name-clause '((:symbol named) ?name))
       ;; TODO not supported: improper list (the "cons")
       (d-type-spec (let* ((cons (cons nil nil))
                           (sub-patterns (vector cons
                                                 (var '?type-specifier)))
                           (pattern (either sub-patterns)))
                      (setf (car cons) pattern
                            (cdr cons) pattern)
                      pattern))
       (destructured-type-spec `((:symbol of-type) d-type-spec))
       (simple-type-spec '(:either fixnum float t nil))
       (type-spec `(:either ,simple-type-spec ,destructured-type-spec))
       (with-clause `((:symbol with)
                      ?*var1
                      ;; TODO (:maybe typespec)
                      ;; this is not currently possible because the matching is too greedy and it's not possible
                      ;; the "typespec" should not match a "=" or "and"
                      (:maybe ((:symbol =) ?*form1))
                      ;; TODO this kind of repetitions could be
                      ;; handled by a special pattern, not sure about
                      ;; the name "joined-repetition"? it's a
                      ;; repitition with a separation (here it's 'and)
                      (:zero-or-more (:symbol and)
                                     ?*var2
                                     ?*form2
                                     ;; (:maybe typespec)
                                     )))
       ;; a compound-form is a non-empty list [which is a form; a
       ;; special form; a lambda form; a macro form; or a function
       ;; form.
       ;; TODO I could use a "one-or-more" here
       (compound-form (vector (repetition (wildcard) :min 1)))
       (compound-form+ (repetition compound-form :min 1))
       (initial-final `((:either (:symbol initially)
                                 (:symbol finally))
                        ,compound-form+))
       (for-as-clause :todo)
       (unconditional `(:either
                        ((:either (:symbol do) (:symbol doing)) ,compound-form+)
                        ((:symbol return)
                         (:either (:symbol it) ?return-form))))
       (list-accumulation `((:either (:symbol collect)
                                     (:symbol collecting)
                                     (:symbol append)
                                     (:symbol appending)
                                     (:symbol nconc)
                                     (:symbol nconcing))
                            (:either (:symbol it) ?form)
                            (:maybe (:symbol into) ,simple-var)))
       (numeric-accumulation `())
       (accumulation `(:either list-accumulation numeric-accumulation))
       (main-clause `(:either
                      unconditional
                      accumulation
                      conditional
                      termination-test
                      ,initial-final))
       (variable-clause `(:either ,with-clause ,initial-final ,for-as-clause))
       (loop-grammar `(loop (:maybe ,@name-clause)
                            (:zero-or-more ,@variable-clause)
                            (:zero-or-more ,@main-clause))))
  ())


#++
(match (compile-pattern loop-grammar)
  #(loop for))
