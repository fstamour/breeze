;;;; Trying to design a DSL for small refactors

(defpackage #:breeze.pattern
  (:documentation "TODO Pattern matching stuff")
  (:use #:cl)
  (:export #:defpattern
           #:match
           #:ref
           #:term
           #:maybe
           #:*match-skip*)
  (:export #:iterate
           #:iterator-done-p))

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

;; Decision: I chose "term" and not "variable" to avoid clashes with
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
            (:constructor maybe (pattern &optional name))
            :constructor
            (:predicate maybep)
            (:include term))
  (pattern nil :read-only t))

(defun maybe= (a b)
  (and (maybep a)
       (maybep b)
       (pattern= (maybe-pattern a)
                 (maybe-pattern b))
       (or (null (maybe-name a))
           (null (maybe-name b))
           (eq (maybe-name a)
               (maybe-name b)))))

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

;; Compile (:maybe ...)
(defmethod compile-compound-pattern ((token (eql :maybe)) pattern)
  ;; TODO check the length of "pattern"
  (maybe (compile-pattern (second pattern)) (third pattern)))

;; Compile (:zero-or-more ...)
(defmethod compile-compound-pattern ((token (eql :zero-or-more)) pattern)
  (zero-or-more (compile-pattern (rest pattern))))

;; Compile (:alternation ...)
(defmethod compile-compound-pattern ((token (eql :alternation)) patterns)
  (alternation (compile-pattern (rest patterns))))



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

;;; TODO the iterator should take care of skipping inputs

(defstruct iterator
  ;; The vector being iterated on
  vector
  ;; The current position in the vector
  (position 0)
  ;; How much to advance the position per iteration
  (step 1)
  ;; The iterator to return when the current one is done
  parent)

(defun iterator-done-p (iterator)
  "Check if there's any values left to iterator over."
  (check-type iterator iterator)
  ;; Simply check if "position" is out of bound.
  (not (< -1
          (iterator-position iterator)
          (length (iterator-vector iterator)))))

(defun iterator-push (iterator vector)
  "Create a new iterator on VECTOR, with ITERATOR as parent. Returns the
new iterator."
  (check-type iterator iterator)
  (check-type vector vector)
  (make-iterator :vector vector :parent iterator))

(defun iterator-maybe-push (iterator)
  "If ITERATOR is not done and the current value is a reference, \"push\"
a new iterator."
  (if (iterator-done-p iterator)
      iterator
      (let ((value (iterator-value iterator)))
        (if (refp value)
            (iterator-maybe-push (iterator-push iterator (ref-pattern value)))
            iterator))))

(defun iterator-maybe-pop (iterator)
  "If ITERATOR is done and has a parent, return the next parent."
  (check-type iterator iterator)
  (if (and (iterator-done-p iterator)
           (iterator-parent iterator))
      (let ((parent (iterator-parent iterator)))
        ;; Advance the position
        (incf (iterator-position parent)
              (iterator-step parent))
        ;; return the parent
        (iterator-maybe-pop parent))
      iterator))

(defun iterate (vector &key (step 1))
  "Create a new iterator."
  (check-type vector vector)
  (let ((iterator
          (iterator-maybe-push
           (make-iterator :vector vector :step step))))
    (if (iterator-skip-p iterator)
        (iterator-next iterator)
        iterator)))

(defvar *match-skip* nil
  "Controls wheter to skip a value when iterating.")

(defun iterator-skip-p (iterator &optional (match-skip *match-skip*))
  (when (and match-skip (not (iterator-done-p iterator)))
    (funcall match-skip (iterator-value iterator))))

(defun %iterator-next (iterator)
  "Advance the iterator. Might return a whole new iterator."
  (check-type iterator iterator)
  ;; Advance the position
  (incf (iterator-position iterator)
        (iterator-step iterator))
  (iterator-maybe-push (iterator-maybe-pop iterator)))

(defun iterator-next (iterator)
  "Advance the iterator. Might return a whole new iterator."
  (check-type iterator iterator)
  (loop :for new-iterator = (%iterator-next iterator)
          :then (%iterator-next new-iterator)
        :while (iterator-skip-p new-iterator)
        :finally (return new-iterator)))

(defun iterator-value (iterator)
  "Get the value at the current ITERATOR's position."
  (check-type iterator iterator)
  (when (iterator-done-p iterator)
    (error "No more values in this iterator."))
  (aref (iterator-vector iterator)
        (iterator-position iterator)))



(defun make-binding (term input)
  (list term input))

(defun merge-bindings (bindings1 bindings2)
  (cond
    ((eq t bindings1) bindings2)
    ((eq t bindings2) bindings1)
    ((or (eq nil bindings1) (eq nil bindings2)) nil)
    (t (append bindings1 bindings2))))

;; Basic "equal" matching
(defmethod match (pattern input)
  (equal pattern input))

;; Match a term (create a binding)
(defmethod match ((pattern term) input)
  (make-binding pattern input))

;; Match a typed term (creates a binding)
(defmethod match ((pattern typed-term) input)
  (when (typep input (typed-term-type pattern))
    (make-binding pattern input)))

;; Recurse into a referenced pattern
(defmethod match ((pattern ref) input)
  (match (ref-pattern pattern) input))

(defmethod match ((pattern maybe) input)
  (or (alexandria:when-let ((bindings (match (maybe-pattern pattern) input)))
        (if (maybe-name pattern)
            (merge-bindings bindings (make-binding pattern input))
            bindings))
      (not input)))

(defmethod match ((pattern alternation) input)
  (some (lambda (pat) (match pat input))
        (alternation-pattern pattern)))

(defmethod match ((pattern zero-or-more) (input null))
  t)

(defmethod match ((pattern zero-or-more) input)
  (match (zero-or-more-pattern pattern) input))

;; TODO This is a mess
(defmethod match ((pattern zero-or-more) (input vector))
  (or (loop
        ;; TODO  (make-empty-bindings)
        :with bindings = t
        :with pat = (zero-or-more-pattern pattern)
        :for guard :below 100 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                              ;; Iterate over the input
        :for input-iterator := (iterate input)
          :then (iterator-next input-iterator)
        :until (iterator-done-p input-iterator)
        ;; save the input iterator's position
        ;; :for input-iterator-position = (iterator-position input-iterator)
        ;; recurse+
        :for new-bindings = (match pat input-iterator)
        :if new-bindings
          ;; collect all the bindings
          :do (setf bindings (merge-bindings bindings new-bindings))
        :else
          ;; failed to match
          :do
             ;; rewind the input iterator
             ;; (setf (iterator-position input-iterator) input-iterator-position)
             ;; bail out of the whole function
             (return-from match nil)
        :finally (return-from match (if (iterator-done-p input-iterator)
                                        nil
                                        bindings)))
      ;; if we get there, it means the pattern matched successfully,
      ;; but there were no new bindings.
      t))

;; Match a string literal
(defmethod match ((pattern string) (input string))
  (string= pattern input))

;; "nil" must match "nil"
(defmethod match ((pattern null) (input null))
  t)

;; TODO I had to do some hackery to "disable" this rule in analysis.lisp
;; the pattern "nil" matches nothing else than "nil"
(defmethod match ((pattern null) input)
  nil)

#++
(defmethod match ((pattern sequence) input)
  (error "Only vector patterns are supported."))

;; Coerce inputs into vectors
(defmethod match ((pattern vector) (input sequence))
  (match pattern (coerce input 'vector)))

;; Match over iterators
(defmethod match ((pattern iterator) (input iterator))
  (match (iterator-value pattern) (iterator-value input)))

(defmethod match ((pattern null) (input iterator))
  (match pattern (iterator-value input)))

(defmethod match ((pattern vector) (input vector))
  (or (loop
        ;; TODO  (make-empty-bindings)
        :with bindings = t
        ;; Iterate over the pattern
        :for pattern-iterator := (iterate pattern)
          :then (iterator-next pattern-iterator)
        :until (iterator-done-p pattern-iterator)
        ;; Iterate over the input
        :for input-iterator := (iterate input)
          :then (iterator-next input-iterator)
        :until (iterator-done-p input-iterator)
        ;; recurse
        :for new-bindings = (match pattern-iterator input-iterator)
        :if new-bindings
          ;; collect all the bindings
          :do (setf bindings (merge-bindings bindings new-bindings))
        :else
          ;; failed to match, bail out of the whole function
          :do (return-from match nil)
        :finally (return-from match (if (iterator-done-p input-iterator)
                                        nil
                                        bindings)))
      ;; if we get there, it means the pattern matched successfully,
      ;; but there were no new bindings.
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
