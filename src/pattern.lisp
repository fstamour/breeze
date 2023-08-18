;;;; Trying to design a DSL for small refactors

(defpackage #:breeze.pattern
  (:documentation "TODO Pattern matching stuff")
  (:use #:cl))

(in-package #:breeze.pattern)

(defvar *patterns* (make-hash-table :test 'equal)
  "Stores all the patterns.")


;;; Refs and Terms

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

(defmethod pattern= ((a vector) (b vector))
  (or (eq a b)
      (and (= (length a) (length b))
           (loop :for x :across a
                 :for y :across b
                 :always (pattern= x y)))))

(macrolet ((def (name)
             `(progn
                (defmethod pattern= ((a ,name) (b ,name))
                  (,(alexandria:symbolicate name '=) a b))
                (defmethod make-load-form ((s ,name) &optional environment)
                  (make-load-form-saving-slots s :environment environment)))))
  (def ref)
  (def term)
  (def typed-term)
  (def maybe)
  (def zero-or-more)
  (def alternation))


;;; Pattern compilation from lists and symbols to vectors and structs

(defun symbol-starts-with (symbol char)
  (and (symbolp symbol)
       (char= char (char (symbol-name symbol) 0))))

(defun term-symbol-p (x)
  (symbol-starts-with x #\?))

;; Default: leave as-is
(defmethod compile-pattern (pattern) pattern)

;; Compile symbols
(defmethod compile-pattern ((pattern symbol))
  (cond
    ((term-symbol-p pattern) (term pattern))
    (t pattern)))

;; Compile lists
(defmethod compile-pattern ((pattern cons))
  ;; Dispatch to another method that is eql-specialized on the firt
  ;; element of the list.
  (compile-compound-pattern (first pattern) pattern))

;; Default list compilation: recurse and convert to vector.
(defmethod compile-compound-pattern (token pattern)
  (map 'vector #'compile-pattern pattern))

;; Compile (:the ...)
(defmethod compile-compound-pattern ((token (eql :the)) pattern)
  ;; TODO Check length of "pattern"
  ;; TODO check if type is nil, that's very likely that's an error.
  (apply #'typed-term (rest pattern)))

;; Compile (:ref ...)
(defmethod compile-compound-pattern ((token (eql :ref)) pattern)
  ;; TODO Check length of "rest"
  (ref (second pattern)))

;; Helper function for compound patterns that can take an arbitrary
;; number of subpatterns.
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

(defun ref-pattern (pattern)
  (check-type pattern ref)
  (or (gethash (ref-name pattern) *patterns*)
      (error "Failed to find the pattern ~S." (ref-name pattern))))


;; Will I regret implemeting this?

(defstruct iterator
  vector
  (position 0)
  (step 1)
  parent)

(defun iterator-done-p (iterator)
  "Check if there's any values left to iterator over."
  (check-type iterator iterator)
  (not (< -1
          (iterator-position iterator)
          (length (iterator-vector iterator)))))

(defun iterator-push (iterator vector)
  (check-type iterator iterator)
  (check-type vector vector)
  (make-iterator :vector vector :parent iterator))

(defun iterator-maybe-push (iterator)
  (if (iterator-done-p iterator)
      iterator
      (let ((value (iterator-value iterator)))
        (if (refp value)
            (iterator-push iterator (ref-pattern value))
            iterator))))

(defun iterator-maybe-pop (iterator)
  (check-type iterator iterator)
  (if (and (iterator-done-p iterator)
           (iterator-parent iterator))
      (iterator-maybe-pop (iterator-parent iterator))
      iterator))

(defun iterate (vector)
  "Create a new iterator."
  (check-type vector vector)
  (iterator-maybe-push (make-iterator :vector vector)))


(defun iterator-next (iterator)
  "Advance the iterator. Might return a whole new iterator."
  (check-type iterator iterator)
  (incf (iterator-position iterator)
        (iterator-step iterator))
  (iterator-maybe-push (iterator-maybe-pop iterator)))

(defun iterator-value (iterator)
  (check-type iterator iterator)
  (when (iterator-done-p iterator)
    (error "No more values in this iterator."))
  (aref (iterator-vector iterator)
        (iterator-position iterator)))



(defmethod match (pattern input)
  (equal pattern input))

(defmethod match ((pattern term) input)
  (cons pattern input))

(defmethod match ((pattern typed-term) input)
  (when (typep input (typed-term-type pattern))
    (cons pattern input)))

#++
(defmethod match ((pattern ref) input)
  (match (ref-pattern pattern) input))

(defmethod match ((pattern string) (input string))
  (string= pattern input))

(defmethod match ((pattern null) (input null))
  t)

(defmethod match ((pattern null) input)
  nil)

#++
(defmethod match ((pattern sequence) input)
  (error "Only vector patterns are supported."))

(defmethod match ((pattern vector) (input sequence))
  (match pattern (coerce input 'vector)))

(defmethod match ((pattern iterator) (input iterator))
  (match (iterator-value pattern) (iterator-value input)))

;; (trace iterator-next iterator-value iterator-push iterator-maybe-pop)

(defmethod match ((pattern vector) (input vector))
  (or (loop
        ;; Iterate over the pattern
        :for pattern-iterator := (iterate pattern) :then (iterator-next pattern-iterator)
        :until (iterator-done-p pattern-iterator)
        ;; :for pat = (iterator-value pattern-iterator)
        ;; Iterate over the input
        :for input-iterator := (iterate input) :then (iterator-next input-iterator)
        :until (iterator-done-p input-iterator)
        ;; :for in = (iterator-value input-iterator)
        ;; recurse
        :for match = #++ (match pat in)
                         (match pattern-iterator input-iterator)
                         ;; debug print
                         ;; :do (format *debug-io* "~%pat: ~s in: ~s" pat in)
        :unless match
          ;; failed to match, bail out of the whole function
          :do (return-from match nil)
        :when (listp match)
          ;; collect all the bindings
          ;; TODO We might want to "merge" the bindings.
          :append match)
      t))


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
