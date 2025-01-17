;;;; Trying to design a DSL for small refactors

(defpackage #:breeze.pattern
  (:documentation "Pattern matching")
  (:use #:cl)
  (:export #:compile-pattern)
  (:export #:defpattern
           #:match
           #:ref
           #:term
           #:maybe
           #:*match-skip*)
  (:export #:iterate
           #:iterator-done-p
           #:iterator-value)
  ;; Working with match results
  (:export #:merge-sets-of-bindings
           #:find-binding
           #:pattern-substitute)
  (:export #:make-rewrite
           #:rewrite-pattern
           #:rewrite-template))

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

(defmethod print-object ((term term) stream)
  (print-unreadable-object
      (term stream :type t :identity t)
    (format stream "~s" (term-name term))))

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


(defstruct (repetition
            (:constructor repetition (pattern min max &optional name))
            :constructor
            (:predicate repetitionp)
            (:include term))
  (pattern nil :read-only t)
  (min nil :read-only t)
  (max nil :read-only t))

(defun repetition= (a b)
  (and (repetitionp a)
       (repetitionp b)
       (pattern= (repetition-pattern a) (repetition-pattern b))
       (= (repetition-min a) (repetition-min b))
       (let ((ma (repetition-max a))
             (mb (repetition-max a)))
         (or (eq ma mb) (and (numberp ma) (numberp mb)) (= ma mb)))
       (or (null (repetition-name a))
           (null (repetition-name b))
           (eq (repetition-name a)
               (repetition-name b)))))

(defun maybe (pattern &optional name)
  (repetition pattern 0 1 name))

(defun zero-or-more (pattern &optional name)
  (repetition pattern 0 nil name))


;;; WIP Alternations

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
  (def repetition)
  (def alternation))


;;; Pattern compilation from lists and symbols to vectors and structs

(defun symbol-starts-with (symbol char)
  (and (symbolp symbol)
       (char= char (char (symbol-name symbol) 0))))

(defun term-symbol-p (x)
  (symbol-starts-with x #\?))

(defparameter *term-pool* nil
  "A pool of terms, used to share terms across patterns created by
independent calls to compile-pattern.")

(defun compile-pattern (pattern)
  "Compiles a PATTERN (specified as a list). Returns 2 values: the
compiled pattern and *term-pool*. If *term-pool* is nil when
compile-pattern is called, a new one is created."
  (if *term-pool*
      (values (%compile-pattern pattern) *term-pool*)
      (let ((*term-pool* (make-hash-table)))
        (compile-pattern pattern))))

;; Default: leave as-is
(defmethod %compile-pattern (pattern) pattern)

;; Compile symbols
(defmethod %compile-pattern ((pattern symbol))
  (cond
    ((term-symbol-p pattern)
     (or (gethash pattern *term-pool*)
         (setf (gethash pattern *term-pool*) (term pattern))))
    (t pattern)))

;; Compile lists
(defmethod %compile-pattern ((pattern cons))
  ;; Dispatch to another method that is eql-specialized on the firt
  ;; element of the list.
  (compile-compound-pattern (first pattern) pattern))

;; Default list compilation: recurse and convert to vector.
(defmethod compile-compound-pattern (token pattern)
  (map 'vector #'%compile-pattern pattern))

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
  (maybe (%compile-pattern (second pattern)) (third pattern)))

;; Compile (:zero-or-more ...)
(defmethod compile-compound-pattern ((token (eql :zero-or-more)) pattern)
  (zero-or-more (%compile-pattern (rest pattern))))

;; Compile (:alternation ...)
(defmethod compile-compound-pattern ((token (eql :alternation)) patterns)
  (alternation (%compile-pattern (rest patterns))))


;;; Re-usable, named patterns

(defmacro defpattern (name &body body)
  `(setf (gethash ',name *patterns*)
         ',(compile-pattern
            (if (breeze.utils:length>1? body)
                body
                (first body)))))

(defun ref-pattern (pattern)
  (check-type pattern ref)
  ;; TODO rename terms????
  (or (gethash (ref-name pattern) *patterns*)
      (error "Failed to find the pattern ~S." (ref-name pattern))))


;;; TODO (important!) replace by iterator.lisp's iterators
;;;
;;; Iterator:
;;;  - takes care of "recursing" into referenced patterns
;;;  - conditionally skips inputs
;;;  - works on vectors only, for my sanity
;;;  - I want to make it possible to iterate backward, hence the "step"

;; Will I regret implemeting this?

(defstruct iterator
  ;; The vector being iterated on
  vector
  ;; The current position in the vector
  (position 0)
  ;; How much to advance the position per iteration
  (step 1)
  ;; The iterator to return when the current one is done
  parent)

#++
(defun iterator-depth (iterator)
  (if (null (iterator-parent iterator))
      0
      (1+ (iterator-depth (iterator-parent iterator)))))

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
  "Advance the iterator exactly once. Might return a whole new iterator."
  (check-type iterator iterator)
  ;; Advance the position
  (incf (iterator-position iterator)
        (iterator-step iterator))
  (iterator-maybe-push (iterator-maybe-pop iterator)))

(defun iterator-next (iterator)
  "Advance the iterator, conditionally skipping some values. Might return
a whole new iterator."
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


;;; Bindings (e.g. the result of a successful match)

(defun make-empty-bindings () t)

(defun make-binding (term input)
  (list (cons term input)))

(defun merge-bindings (bindings1 bindings2)
  (flet ((name (x)
           (if (termp x) (term-name x) x)))
    (cond
      ((eq t bindings1) bindings2)
      ((eq t bindings2) bindings1)
      ((or (eq nil bindings1) (eq nil bindings2)) nil)
      (t
       ;; TODO It would be possible to pass the bindings into all "match"
       ;; functions and methods. It would allow to detect conflicting
       ;; bindings earlier and stop the matching process earlier.
       ;;
       ;; N.B. a disjoint-set data structure could help detect cycles in
       ;; the bindings.
       ;;
       ;; TODO use a hash-table ffs
       (delete-duplicates
        (sort (append bindings1 bindings2)
              (lambda (a b)
                (let ((na (name (car a)))
                      (nb (name (car b))))
                  (if (string= na nb)
                      (unless (eql (cdr a) (cdr b))
                        (return-from merge-bindings nil))
                      ;; (error "Conflicting bindings: ~a ~a" a b)
                      (string< na nb)))))
        :key (alexandria:compose #'name #'car)
        :test #'string=)))))

(defun merge-sets-of-bindings (set-of-bindings1 set-of-bindings2)
  "Merge two set of bindings (list of list of bindings), returns a new
set of bindings.
Matching a pattern against a set of values (e.g. an egraph) will yield
a set of independant bindings. During the macthing process, we might
need to refine the \"current\" set of bindings. Long-story short, this
is analoguous to computing the Cartesian product of the two sets of
bindings and keeping only those that have not conflicting bindings."
  (loop :for bindings1 :in set-of-bindings1
        :append (loop :for bindings2 :in set-of-bindings2
                      :for merged-bindings = (breeze.pattern::merge-bindings
                                              bindings1 bindings2)
                      :when merged-bindings
                        :collect merged-bindings)))

(defun find-binding (bindings term-or-term-name)
  (when bindings
    (if (termp term-or-term-name)
        (assoc term-or-term-name bindings)
        (assoc term-or-term-name bindings
               :key #'term-name))))


;;; Matching atoms

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

;; Match a string literal
(defmethod match ((pattern string) (input string))
  (string= pattern input))

;; "nil" must match "nil"
(defmethod match ((pattern null) (input null))
  t)

;; "nil" must not match any other symbols
(defmethod match ((pattern null) (input symbol))
  nil)


;;; Matching sequences

(defmethod match ((pattern iterator) (input iterator))
  (loop
    :with bindings = (make-empty-bindings)
    ;; Iterate over the pattern
    :for pattern-iterator := pattern
      :then (iterator-next pattern-iterator)
    ;; Iterate over the input
    :for input-iterator := input
      :then (iterator-next input-iterator)
    :until (or (iterator-done-p pattern-iterator)
               (iterator-done-p input-iterator))
    :for new-bindings = (match
                            (iterator-value pattern-iterator)
                          (iterator-value input-iterator))
    :if new-bindings
      ;; collect all the bindings
      :do
         ;; (break)
         (setf bindings (merge-bindings bindings new-bindings))
         ;; The new bindings conflicted with the existing ones...
         (unless bindings (return nil))
    :else
      ;; failed to match, bail out of the whole function
      :do (return nil)
    :finally
       ;; We advance the input iterator to see if there are still
       ;; values left that would not be skipped.
       (when (and (not (iterator-done-p input-iterator))
                  (iterator-skip-p input-iterator))
         (setf input-iterator (iterator-next input-iterator)))
       (return
         ;; We want to match the whole pattern, but wheter we
         ;; want to match the whole input is up to the caller.
         (when (iterator-done-p pattern-iterator)
           (values (or bindings t)
                   (if (iterator-done-p input-iterator)
                       nil
                       input-iterator))))))

(defmethod match ((pattern term) (input iterator))
  (multiple-value-bind (bindings input-remaining-p)
      (match (iterate (vector pattern)) input)
    (unless input-remaining-p
      bindings)))

(defmethod match ((pattern vector) (input vector))
  (multiple-value-bind (bindings input-remaining-p)
      (match (iterate pattern) (iterate input))
    (unless input-remaining-p
      bindings)))


;;; Matching alternations

(defmethod match ((pattern alternation) input)
  (some (lambda (pat) (match pat input))
        (alternation-pattern pattern)))


;;; Matching repetitions

#++
(defmethod match ((pattern maybe) input)
  (or (alexandria:when-let ((bindings (match (maybe-pattern pattern) input)))
        (if (maybe-name pattern)
            (merge-bindings bindings (make-binding pattern input))
            bindings))
      (not input)))

#++
(defmethod match ((pattern zero-or-more) (input null))
  t)

(defmethod match ((pattern repetition) (input vector))
  (loop
    :with bindings = (make-empty-bindings)
    :with pat = (repetition-pattern pattern)
    :with input-iterator := (iterate input)
    :for i :from 0
    :do (multiple-value-bind (new-bindings new-input-iterator)
            (match (iterate pat) input-iterator)
          ;; (break)
          (if new-bindings
              ;; collect all the bindings (setf bindings
              ;; (merge-bindings bindings new-bindings))
              (progn
                (setf bindings (merge-bindings bindings new-bindings))
                ;; TODO check if bindings is nil after merging.
                )
              ;; No match
              (if (<= (repetition-min pattern) i)
                  (return bindings)
                  (return nil)))
          (if new-input-iterator
              (setf input-iterator new-input-iterator)
              ;; No more input left
              (if (<= (repetition-min pattern) i)
                  (return bindings)
                  (return nil))))))

;; TODO
;; (defmethod match ((pattern repetition) (input iterator)))


;;; Convenience automatic coercions

(defmethod match ((pattern vector) (input iterator))
  (match (iterate pattern) input))

(defmethod match ((pattern vector) (input sequence))
  (match pattern (coerce input 'vector)))

(defmethod match ((pattern repetition) (input sequence))
  (match pattern (coerce input 'vector)))



;;; Match substitution

(defun pattern-substitute (pattern bindings &optional (result-type 'vector))
  (when pattern
    ;; Patterns are never compiled to lists
    (check-type pattern atom)
    (flet ((substitute1 (x)
             (etypecase x
               (term
                (alexandria:if-let ((binding (find-binding bindings x)))
                  (cdr binding)
                  ;; TODO this could signal a condition (binding not
                  ;; found)
                  x))
               ((or symbol number) x))))
      (if (vectorp pattern)
          (map result-type
               ;; Note: we could've use map to recurse directly into
               ;; pattern-subtitute, but not doing so make tracing
               ;; (and debugging) tremenduously easier.
               #'(lambda (subpattern)
                   (if (vectorp subpattern)
                       (pattern-substitute subpattern bindings result-type)
                       (substitute1 subpattern)))
               pattern)
          (substitute1 pattern)))))



;;; Rules and rewrites


;; TODO "rules" would be "bidirectional" and "rewrites" wouldn't.
;; TODO (defun rule (a b) ...)
;; TODO (defun make-rewrite (antecedant consequent) ...)

#++ (progn
      (defclass abstract-rule () ())

      (defclass rule (abstract-rule) ())

      (defun make-rule (a b)
        (let ((*term-pool* (make-hash-table)))
          (list :rule
                (compile-pattern a)
                (compile-pattern b))))

      (defun make-rewrite (a b)
        (let ((*term-pool* (make-hash-table)))
          (list :rewrite
                (compile-pattern a)
                (compile-pattern b)))))

(defun make-rewrite (pattern template)
  (let ((*term-pool* (make-hash-table)))
    (cons
     (compile-pattern pattern)
     (compile-pattern template))))

(defun rewrite-pattern (rewrite)
  "Get the pattern of a REWRITE rule."
  (car rewrite))

(defun rewrite-template (rewrite)
  "Get the template of a REWRITE rule."
  (cdr rewrite))
