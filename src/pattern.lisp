;;;; Trying to design a DSL for small refactors

(defpackage #:breeze.pattern
  (:documentation "TODO Pattern matching stuff")
  (:use #:cl))

(in-package #:breeze.pattern)

(defvar *patterns* (make-hash-table :test 'equal)
  "Stores all the patterns.")



;; TODO Think about (defstruct hole ...)

;; Used to reference another pattern by name.
(defstruct (ref
            (:constructor ref (name))
            :constructor
            (:predicate refp))
  (name nil :type symbol :read-only t))

(defun ref= (a b)
  (and (refp a)
       (refp b)
       (eq (ref-name a)
           (ref-name b))))

;; Decision: I chose "term" and not "variable" to avoid clashed with
;; cl:variable
(defstruct (term
            (:constructor term (name))
            :constructor
            (:predicate termp))
  (name nil :type symbol :read-only t))

(defun term= (a b)
  (and (termp a)
       (termp b)
       (eq (term-name a)
           (term-name b))))

(defstruct (typed-term
            (:constructor typed-term (type name))
            :constructor
            :predicate
            (:include term))
  (type nil :read-only t))

(defmethod typed-term= (a b) nil)

(defmethod typed-term= ((a term) (b term))
  (term= a b))

(defmethod typed-term= ((a typed-term) (b typed-term))
  (and (eq (typed-term-name a)
           (typed-term-name b))
       (equal (typed-term-type a)
              (typed-term-type b))))



;; TODO Maybe generalize "maybe" and "zero-or-more" into "repetition"

(defstruct (maybe
            (:constructor maybe (pattern))
            :constructor
            (:predicate maybep))
  (pattern nil :read-only t))

(defun maybe= (a b)
  (and (maybep a)
       (maybep b)
       (pattern= (maybe-pattern a)
                 (maybe-pattern b))))

(defstruct (zero-or-more
            (:constructor zero-or-more (pattern))
            :constructor
            :predicate)
  (pattern nil :read-only t))

(defun zero-or-more= (a b)
  (and (zero-or-more-p a)
       (zero-or-more-p b)
       (pattern= (zero-or-more-pattern a)
                 (zero-or-more-pattern b))))

(defstruct (alternation
            (:constructor alternation (pattern))
            :constructor
            (:predicate alternationp))
  (pattern nil :read-only t))

(defun alternation= (a b)
  (and (alternationp a)
       (alternationp b)
       (pattern= (alternation-pattern a)
                 (alternation-pattern b))))

(defmethod pattern= (a b)
  (equal a b))

(macrolet ((def= (name)
             `(defmethod pattern= ((a ,name) (b ,name))
                (,(alexandria:symbolicate name '=) a b))))
  (def= ref)
  (def= term)
  (def= typed-term)
  (def= maybe)
  (def= zero-or-more)
  (def= alternation))

(macrolet ((def (name)
             `(defmethod make-load-form ((s ,name) &optional environment)
                (make-load-form-saving-slots s :environment environment))))
  (def ref)
  (def term)
  (def typed-term)
  (def maybe)
  (def zero-or-more)
  (def alternation))



(defun symbol-starts-with (symbol char)
  (and (symbolp symbol)
       (char= char (char (symbol-name symbol) 0))))

(defun symbol-name-butfirst (symbol)
  (let ((name (symbol-name symbol)))
    (subseq name 1 (1- (length name)))))

(defun term-symbol-p (x)
  (symbol-starts-with x #\?))

#++
(defun ref-symbol-p (x)
  (symbol-starts-with x #\$))

;; Default compile
(defmethod compile-pattern (pattern) pattern)

;; Compile symbols
(defmethod compile-pattern ((pattern symbol))
  (cond
    ((term-symbol-p pattern) (term pattern))
    #++ ((ref-symbol-p pattern) (ref pattern))
    (t pattern)))

;; Compile lists
(defmethod compile-pattern ((pattern cons))
  (compile-compound-pattern (first pattern) pattern))

;; Default list compilation
(defmethod compile-compound-pattern (token pattern)
  (mapcar #'compile-pattern pattern))

;; Compile (:the ...)
(defmethod compile-compound-pattern ((token (eql :the)) pattern)
  ;; TODO Check length of "pattern"
  ;; TODO check if type is nil, that's very likely that's an error.
  (apply #'typed-term (rest pattern)))

(defmethod compile-compound-pattern ((token (eql :ref)) pattern)
  ;; TODO Check length of "rest"
  (ref (second pattern)))

(defun rest-or-second (list)
  (if (cddr list) (rest list) (second list)))
;; (rest-or-second '(a b c)) => '(b c)
;; (rest-or-second '(a b)) => 'b

;; Compile (:maybe ...)
(defmethod compile-compound-pattern ((token (eql :maybe)) pattern)
  (maybe (compile-pattern (rest-or-second pattern))))

;; Compile (:zero-or-more ...)
(defmethod compile-compound-pattern ((token (eql :zero-or-more)) pattern)
  (zero-or-more (compile-pattern (rest-or-second pattern))))

;; Compile (:alternation ...)
(defmethod compile-compound-pattern ((token (eql :alternation)) patterns)
  (alternation (compile-pattern (rest-or-second patterns))))



(defmacro defpattern (name &body body)
  `(setf (gethash ',name *patterns*)
         ',(compile-pattern
            (if (breeze.utils:length>1? body)
                body
                (first body)))))



(defmethod iterate ((sequence list))
  sequence)

(defmethod iterator-next ((iterator list) (sequence list))
  (declare (ignore sequence))
  (cdr iterator))

(defmethod iterator-value ((iterator list) (sequence list))
  (declare (ignore sequence))
  (car iterator))

(defmethod iterator-done-p ((iterator list) (sequence list))
  (null iterator))



(defmethod iterate ((sequence vector))
  0)

(defmethod iterator-next ((iterator integer) (sequence vector))
  (declare (ignore sequence))
  (incf iterator))

(defmethod iterator-value ((iterator integer) (sequence vector))
  (aref sequence iterator))

(defmethod iterator-done-p ((iterator integer) (sequence vector))
  (>= iterator (length sequence)))



(defmethod match (pattern input)
  (equal pattern input))

(defmethod match ((pattern term) input)
  (cons pattern input))

(defmethod match ((pattern typed-term) input)
  (when (typep input (typed-term-type pattern))
    (cons pattern input)))

(defmethod match ((pattern ref) input)
  (match (gethash (ref-name pattern) *patterns*) input))


(defmethod match ((pattern string) (input string))
  (string= pattern input))

(defmethod match ((pattern null) (input null))
  t)

(defmethod match ((pattern null) input)
  nil)



(defmethod match ((pattern sequence) input)
  (loop
    :for pattern-iterator := (iterate pattern) :then (iterator-next pattern-iterator pattern)
    :while (not (iterator-done-p pattern-iterator pattern))
    :for pat = (iterator-value pattern-iterator pattern)
    :for input-iterator := (iterate input) :then (iterator-next input-iterator input)
    :while (not (iterator-done-p input-iterator input))
    :for in = (iterator-value input-iterator input)
    :for match = (match pat in)
    :do (format *debug-io* "~%pat: ~s in: ~s" pat in)
    :unless match
      :do (return-from match nil)
    :when (listp match)
      :append match)
  t)


#++
(defmethod skip-input-p (x)
  (and (symbolp x)
       (char= #\< (char (symbol-name x) 0))))

#++
(defmethod match ((pattern vector) (input vector)
                  &aux (i 0) (j 0))
  (labels
      ((pattern () (aref pattern i))
       (input () (aref input j))
       (advance-pattern () (incf i))
       (advance-input ()
         (loop
           :do (incf j)
           :while (and (< j (length input))
                       (skip-input-p (input))))))
    (loop
      :for guard :below 1000
      :for (match . bindings) = (multiple-value-list
                                 (match (pattern) (input)))
      :unless match
        :return nil
      :append bindings
      :do (advance-pattern)
      :while (< i (length pattern))
      :do (advance-input)
      :while (< j (length input)))))

#++
(let ((pattern
        (list-vector `(defun ?name ?ordinary-lambda-list ?body)))
      (input
        (list-vector `(defun <ws> foo <ws> (x <ws> y) <nl> <ws> (+ <ws> x <ws> y)))))
  (match pattern input))
