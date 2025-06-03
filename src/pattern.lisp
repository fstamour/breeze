;;;; Trying to design a DSL for small refactors

(defpackage #:breeze.pattern
  (:documentation "Pattern matching")
  (:use #:cl #:breeze.iterator)
  (:import-from #:breeze.string
                #:symbol-starts-with)
  (:export #:compile-pattern)
  (:export #:defpattern
           #:match
           #:ref
           #:term
           #:maybe)
  ;; Working with match results
  (:export #:merge-sets-of-bindings
           #:find-binding
           #:binding
           #:from
           #:to
           #:binding-set
           #:binding-set-p
           #:pattern-substitute)
  (:export #:make-rewrite
           #:rewrite-pattern
           #:rewrite-template))

(in-package #:breeze.pattern)


;;; Refs

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


;;; Terms

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


;;; Typed-terms

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


;;; Repetitions

(defstruct (repetition
            (:constructor repetition (pattern min max &optional name))
            :constructor
            (:predicate repetitionp)
            (:include term))
  (pattern nil :read-only t)
  (min nil :read-only t)
  (max nil :read-only t)
  ;; TODO (greedyp t :read-only t)
  )

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


;;; Pattern comparison

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

(defun term-symbol-p (x)
  (symbol-starts-with x #\?))

(defparameter *term-pool* nil
  "A pool of terms, used to share terms across patterns created by
independent calls to compile-pattern.")

(defun make-term-pool () (make-hash-table))

(defun compile-pattern (pattern)
  "Compiles a PATTERN (specified as a list). Returns 2 values: the
compiled pattern and *term-pool*. If *term-pool* is nil when
compile-pattern is called, a new one is created."
  (if *term-pool*
      (values (%compile-pattern pattern) *term-pool*)
      (let ((*term-pool* (make-term-pool)))
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

(defvar *patterns* (make-hash-table :test 'equal)
  "Stores all the patterns.")

(defmacro defpattern (name &body body)
  `(setf (gethash ',name *patterns*)
         ',(compile-pattern
            (if (breeze.utils:length>1? body)
                body
                (first body)))))

(defun ref-pattern (pattern)
  (check-type pattern (or ref symbol))
  ;; TODO rename terms????
  (or (typecase pattern
        (symbol (gethash pattern *patterns*))
        (ref (gethash (ref-name pattern) *patterns*)))
      (error "Failed to find the pattern ~S." (ref-name pattern))))


;;; Bindings (e.g. the result of a successful match)

(defclass binding ()
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to))
  (:documentation "A binding"))

(defun make-binding (term input)
  (make-instance 'binding :from term :to input))

(defun bindingp (x)
  (typep x 'binding))

(defmethod print-object ((binding binding) stream)
  (print-unreadable-object
      (binding stream :type t)
    (format stream "~s → ~s" (from binding) (to binding))))

(defclass binding-set ()
  ((bindings
    :initform (make-hash-table)
    :initarg :bindings
    :reader bindings))
  ;; TODO add a union-hash to detect cycles...
  (:documentation "A set of bindings"))

(defun binding-set-p (x)
  (or (eq x t)
      (typep x 'binding-set)))

(defmethod print-object ((binding-set binding-set) stream)
  (print-unreadable-object
      (binding-set stream :type t)
    (let* ((bindings (bindings binding-set))
           (size (hash-table-count bindings)))
      (cond
        ((zerop size) (write-string "(empty)" stream) )
        ((= 1 size) (prin1 (alexandria:hash-table-alist bindings) stream))
        (t (format stream "(~d bindings)" size))))))

(defmethod emptyp ((binding-set binding-set))
  (zerop (hash-table-count (bindings binding-set))))

;; I think using the convention of "T represents an empty binding set,
;; which represents a successful match without captures" is nice, the
;; GC might like it too...
(defmethod emptyp ((binding-set (eql t)))
  t)

(defmethod emptyp ((binding binding))
  nil)

(defun make-binding-set (&key bindings)
  (if bindings
    (make-instance 'binding-set :bindings bindings)
    (make-instance 'binding-set)))

(defun copy-binding-set (binding-set)
  (make-binding-set
   :bindings (alexandria:copy-hash-table (bindings binding-set))))

(defun find-binding (binding-set from)
  (gethash from (bindings binding-set)))

;; TODO maybe this could be a method instead of a defun?
(defun set-binding (binding-set binding)
  (setf (gethash (from binding) (bindings binding-set)) binding))

(defmethod add-binding ((binding-set binding-set) (_ (eql t)))
  ;; nothing to do
  t ; success
  )

(defmethod add-binding ((binding-set binding-set) (_ (eql nil)))
  ;; nothing to do
  nil ; failure
  )

(defmethod add-binding ((binding-set binding-set) (new-binding binding))
  (let ((old-binding (find-binding binding-set (from new-binding))))
    (and (if old-binding
             ;; (error "Conflicting bindings: ~a ~a" a b)
             (eql (to old-binding) (to new-binding))
             (set-binding binding-set new-binding))
         binding-set)))

(defun ensure-binding-set (x)
  (etypecase x
    (binding-set x)
    (binding (let ((binding-set (make-binding-set)))
               (add-binding binding-set x)
               binding-set))
    (null nil)
    ((eql t) t)))

(defun merge-bindings (bindings1 bindings2)
  (cond
    ((or (null bindings1) (null bindings2)) nil)
    ((and (eq t bindings1) (eq t bindings2)) t)
    ;; when merging two binding instances instead of binding-sets
    ((and (bindingp bindings1) (bindingp bindings2))
     (let ((result (make-binding-set)))
       (add-binding result bindings1)
       (when (add-binding result bindings2)
         result)))
    ((emptyp bindings1) (ensure-binding-set bindings2))
    ((emptyp bindings2) (ensure-binding-set bindings1))
    ;; 1 is instance, 2 is set
    ((bindingp bindings1)
     (add-binding bindings2 bindings1))
    ;; 2 is instance, 1 is set
    ((bindingp bindings2)
     (add-binding bindings1 bindings2))
    (t
     ;; TODO It would be possible to pass the bindings into all "match"
     ;; functions and methods. It would allow to detect conflicting
     ;; bindings earlier and stop the matching process earlier.
     ;;
     ;; N.B. a disjoint-set data structure could help detect cycles in
     ;; the bindings.
     (let ((result (copy-binding-set bindings1)))
       (loop :for from2 :being :the :hash-key :of bindings2 :using (hash-value binding2)
             :for successp = (add-binding result binding2)
             :unless successp :do (return))
       result))))


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


;;; Matching

(defgeneric match (pattern input &key))


;;; Matching atoms

;; Basic "equal" matching
(defmethod match (pattern input &key &allow-other-keys)
  (equal pattern input))

;; Match a term (create a binding)
(defmethod match ((pattern term) input &key)
  (make-binding pattern input))

;; Match a typed term (creates a binding)
(defmethod match ((pattern typed-term) input &key)
  (when (typep input (typed-term-type pattern))
    (make-binding pattern input)))

;; Recurse into a referenced pattern
(defmethod match ((pattern ref) input &key)
  (match (ref-pattern pattern) input))

;; Match a string literal
(defmethod match ((pattern string) (input string) &key)
  (string= pattern input))

;; "nil" must match "nil"
(defmethod match ((pattern null) (input null) &key)
  t)

;; "nil" must not match any other symbols
(defmethod match ((pattern null) (input symbol) &key)
  nil)


;;; Iterators

(defclass pattern-iterator (leaf-iterator)
  ())

(defmethod make-pattern-iterator ((pattern vector))
  (make-leaf-iterator
   pattern
   (lambda (pattern)
     (and (refp pattern)
          (ref-pattern pattern)))
   :class 'pattern-iterator))


;;; Matching sequences

(defmethod match (($pattern pattern-iterator) ($input iterator) &key skipp)
  (loop
    :with bindings = t ;; (make-binding-set)
    :until (or (donep $pattern) (donep $input))
    :for new-bindings = (progn
                          (when skipp
                            (loop :while (and
                                          (not (donep $input))
                                          (funcall skipp $input))
                                  :do (next $input :dont-recurse-p t)))
                          (when (donep $input) (return))
                          (let ((sub-pattern (value $pattern)))
                                (when (vectorp sub-pattern)
                                    (push-vector $pattern sub-pattern)
                                    (go-down $input)
                                    (when (donep $input) (return))))
                          (match
                              (value $pattern)
                            ;; match-symbol-to-token would like an
                            ;; iterator (to access the state, but
                            ;; otherwise, this causes infinite
                            ;; recursion.
                            ;; TODO this is a very bad hack
                            (if (typep $input 'breeze.lossless-reader::node-iterator )
                                $input
                                (value $input))))
    :do (next $pattern) (next $input :dont-recurse-p t)
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
       (when (and skipp
                  (not (donep $input))
                  (funcall skipp $input))
         (next $input :dont-recurse-p t))
       (return
         ;; We want to match the whole pattern, but wheter we
         ;; want to match the whole input is up to the caller.
         (when (donep $pattern)
           (values (or bindings t)
                   (unless (donep $input) $input))))))

(defmethod match ((pattern term) (input iterator) &key skipp)
  (multiple-value-bind (bindings input-remaining-p)
      (match (make-pattern-iterator (vector pattern)) input :skipp skipp)
    (unless input-remaining-p
      bindings)))

(defmethod match ((pattern vector) (input vector) &key skipp)
  (multiple-value-bind (bindings input-remaining-p)
      (match (make-pattern-iterator pattern)
        (make-vector-iterator input)
        :skipp skipp)
    (unless input-remaining-p
      bindings)))


;;; Matching alternations

(defmethod match ((pattern alternation) input &key)
  (some (lambda (pat) (match pat input))
        (alternation-pattern pattern)))


;;; Matching repetitions

(defmethod match ((pattern repetition) (input vector) &key skipp)
  (loop
    :with bindings = (make-binding-set)
    :with pat = (repetition-pattern pattern)
    :with input-iterator := (make-vector-iterator input)
    :for i :from 0
    :do (multiple-value-bind (new-bindings new-input-iterator)
            (match (make-pattern-iterator pat) input-iterator :skipp skipp)
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

(defmethod match ((pattern vector) (input iterator) &key skipp)
  (go-down input)
  (unless (donep input)
    (match (make-pattern-iterator pattern) input :skipp skipp)))

(defmethod match ((pattern vector) (input sequence) &key skipp)
  (match pattern (coerce input 'vector) :skipp skipp))

(defmethod match ((pattern repetition) (input sequence) &key skipp)
  (match pattern (coerce input 'vector) :skipp skipp))


;;; Match substitution

(defun pattern-substitute (pattern bindings &optional (result-type 'vector))
  (when pattern
    ;; Patterns are never compiled to lists
    (check-type pattern atom)
    (flet ((substitute1 (x)
             (etypecase x
               (term
                (alexandria:if-let ((binding (find-binding bindings x)))
                  (to binding)
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
        (let ((*term-pool* (make-term-pool)))
          (list :rule
                (compile-pattern a)
                (compile-pattern b))))

      (defun make-rewrite (a b)
        (let ((*term-pool* (make-term-pool)))
          (list :rewrite
                (compile-pattern a)
                (compile-pattern b)))))

(defun make-rewrite (pattern template)
  (let ((*term-pool* (make-hash-table)))
    (cons ;; TODO use a class instead
     (compile-pattern pattern)
     (compile-pattern template))))

(defun rewrite-pattern (rewrite)
  "Get the pattern of a REWRITE rule."
  (car rewrite))

(defun rewrite-template (rewrite)
  "Get the template of a REWRITE rule."
  (cdr rewrite))
