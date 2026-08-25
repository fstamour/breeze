(in-package #:breeze.pattern)

;; TODO find-match
;; TODO find-partial-match
;; TODO fuzzy match (e.g. symbol with a low edit distance is considerded a match)

;; TODO callbacks? instead of bindings??

;; TODO match-state object, should include the skipp predicate; the
;; pattern and input iterators

;; TODO condition failed-match


;;; Matching

(defgeneric match (pattern input &key))


;;; Debugging

(defvar *debug-match* nil)
(defvar *debug-match-depth* -1)
(setf *debug-match-depth* -1)

(defmethod match :around (pattern input &rest kargs)
  (if *debug-match*
      ;; TODO only print before/after when they (pattern or input) are iteratorp
      ;; (or (iteratorp pattern) (iteratorp input))
      (let* ((*debug-match-depth* (1+ *debug-match-depth*))
             (*print-pretty* nil)
             (*print-length* nil)
             (stream *standard-output* #| *trace-output* |#)
             (per-line-prefix (format nil "~v@{~A~:*~}" *debug-match-depth* "  |")))
        (flet ((frob (control &rest args)
                 (fresh-line stream)
                 (pprint-logical-block (stream nil :per-line-prefix per-line-prefix)
                   (apply #'format stream control args))))
          ;; before
          (frob "~3d (match ~a ~a ~s)" *debug-match-depth* pattern input kargs)
          (let* ((results (multiple-value-list (call-next-method))))
            ;; after
            (frob "~3d => ~{~a~^, ~}" *debug-match-depth* results)
            ;; return
            (apply #'values results))))
      (call-next-method)))

#++
(let ((*debug-match* t))
  (with-output-to-string (*trace-output*)
    (match 'x nil)))


;;; Matching atoms

(defmethod match (pattern input &key &allow-other-keys)
  "Basic \"equal\" matching"
  (equal pattern input))

(defmethod match ((pattern wildcard) input &key &allow-other-keys)
  "Match anything."
  (declare (ignore input))
  t)

(defmethod match ((pattern wildcard) (input iterator) &key &allow-other-keys)
  "Wildcard does not match a iterator that is donep (there's nothing to match)."
  (unless (donep input)
    (next input)
    t))

(defmethod match ((var simple-var) input &key)
  "Match a var (create a binding)"
  (make-binding var input))

(defmethod match ((var var) input &key skipp)
  "Match a var (create a binding)"
  (let ((sub-bindings (match (pattern var) input :skipp skipp)))
    (when sub-bindings
      (make-binding var input
                    :pattern var
                    :children sub-bindings))))

(defmethod match ((var-a var) (var-b var) &key skipp)
  "Match a var (create a binding)"
  (let ((sub-bindings (match (pattern var-a) (pattern var-b) :skipp skipp)))
    (when sub-bindings
      ;; TODO do we really need to pass var-a twice?
      (make-binding var-a var-b
                    :pattern var-a
                    :children sub-bindings))))

;; smells like unification
(defmethod match ((var1 simple-var) (var2 simple-var) &key)
  "Match a var against another var."
  ;; TODO check multi-valued-p ?
  (make-binding (name var1) var2))

(defmethod match ((pattern string) (input string) &key)
  "Match a string literal"
  (string= pattern input))

;; "nil" must match "nil"
(defmethod match ((pattern null) (input null) &key)
  "Match `nil' against `nil'."
  t)

;; TODO regex???
(defmethod match ((pattern sym) (symbol symbol) &key)
  "Match a pattern of type `sym' against a symbol."
  (with-slots ((symbol-name-pattern name)
               (package-name-pattern package))
      pattern
    (let* ((package (symbol-package symbol))
           (symbol-name (symbol-name symbol)))
      (flet ((match-package ()
               ;; assumes that package-name-pattern is not :wild
               (cond
                 ((null package) (null package-name-pattern))
                 ((null package-name-pattern) nil)
                 (t (alexandria:when-let ((pattern-package (find-package package-name-pattern)))
                      ;; TODO package-local-nicknames
                      (or (eq pattern-package package)
                          ;; handle re-exported symbols
                          (eq symbol (find-symbol symbol-name pattern-package)))))))
             (match-symbol ()
               ;; assumes that symbol-name-pattern is not :wild
               (string= (string symbol-name-pattern) symbol-name)))
        (cond
          ((and (wildp symbol-name-pattern)
                (wildp package-name-pattern))
           t)
          ((wildp symbol-name-pattern) (match-package))
          ((wildp package-name-pattern) (match-symbol))
          (t (and (match-package) (match-symbol))))))))


;;; Iterators

(defclass pattern-iterator (tree-iterator)
  ())

(defmethod make-pattern-iterator ((pattern vector))
  (make-instance 'pattern-iterator :root pattern))

(defmethod make-pattern-iterator ((pattern t))
  (make-pattern-iterator (vector pattern)))

(defmethod children ((iterator pattern-iterator))
  (unless (tree-iterator-done-p iterator)
    (let ((value (tree-iterator-value iterator)))
      (typecase value
        (vector value)
        (pattern (children value))))))


;;; Matching sequences

(defmethod match ((pattern-iterator pattern-iterator) (iterator tree-iterator) &key skipp)
  (loop
    :with bindings = t ;; (make-substitutions)
    :with new-bindings = nil
    :with $pattern :of-type pattern-iterator := (copy-iterator pattern-iterator)
    :with $input :of-type tree-iterator := (copy-iterator iterator)
    :until (tree-iterator-done-p $pattern)
    :for i :from 0
    :do
       (skip $input skipp)
       (let* ((sub-pattern (tree-iterator-value $pattern)))
         (setf new-bindings
               (match sub-pattern $input :skipp skipp))
         ;; if pattern was a repetition, then the iterator will
         ;; already have been updated.
         (unless (or (wildcardp sub-pattern) (repetitionp sub-pattern))
           (tree-iterator-next $input)))
       (tree-iterator-next $pattern)
       ;; infinite loop guard
       (when (<= 1000000 i)
         (error "Infinite loop? in (match pattern-iterator tree-iterator). $input=pos~a" (pos $input)))
       (cond
         ;; collect all the bindings
         (new-bindings
          (setf bindings (merge-substitutions bindings new-bindings))
          ;; The new bindings conflicted with the existing ones...
          (unless bindings (return (values nil :conflicting-bindings))))
         ;; failed to match, bail out of the whole function
         (t (return (values nil :failed-submatch))))
    :finally
       (return
         ;; We want to match the whole pattern, but whether we want
         ;; to match the whole input is up to the caller. So we
         ;; don't check if $input is done.
         (cond
           ((tree-iterator-done-p $pattern)
            ;; update the input iterator
            (copy-iterator $input iterator)
            ;; update the pattern iterator
            (copy-iterator $pattern pattern-iterator)
            bindings)
           (t (values nil :pattern-not-done $pattern (tree-iterator-value $pattern)))))))

(defmethod match ((var simple-var) ($input iterator) &key skipp)
  "Match a `var' pattern against the current value of an
`iterator'. This always match, it advances the iterator and returns a
binding."
  (let (($it (copy-iterator $input)))
    (skip $it skipp)
    (unless (donep $it)
      (copy-iterator $it $input)
      (make-binding var $it))))

(defmethod match ((var var) ($input iterator) &key skipp)
  "Match a `var' pattern against the current value of an
`iterator'. This always match, it advances the iterator and returns a
binding."
  (let (($it (copy-iterator $input)))
    (skip $it skipp)
    (unless (donep $it)
      (let ((sub-bindings (match (pattern var) $it)))
        (when sub-bindings
          (copy-iterator $it $input)
          (make-binding (name var) $it
                        :pattern var
                        ;; TODO we might want to control whether these
                        ;; child bindings will be merged in the
                        ;; callers' substitutions (set of bindings)
                        :children sub-bindings))))))

(defmethod match (($pattern t) ($input iterator) &key skipp)
  "Fallback: match something against the current value of the `iterator'
$INPUT."
  (skip $input skipp)
  (unless (donep $input)
    (match $pattern (value $input))))

(defmethod match ((pattern vector) (input vector) &key skipp)
  "Convenience: create an iterator on INPUT, match PATTERN against it and
return two values: the match result and the iterator on the
input (which can then be used by the caller to verify if all input
where consumed, for example)."
  (let (($pattern (make-pattern-iterator pattern))
        ($input (make-tree-iterator input)))
    (values (match $pattern $input :skipp skipp)
            $input)))

(defmethod match ((pattern vector) (input iterator) &key skipp)
  (let (($input (copy-iterator input)))
    (go-down $input)
    (unless (donep $input)
      ;; TODO return all the values returned by `match'
      (let ((substitution (match (make-pattern-iterator pattern) $input :skipp skipp)))
        (when substitution
          (copy-iterator $input input)
          (go-up input))
        substitution))))


;;; Matching eithers

;; TODO add tests with and without skipp
(defmethod match ((pattern either) input &key skipp)
  "Match an `either' pattern against INPUT."
  (loop :for pat :across (patterns pattern)
        :for bindings = (match pat input :skipp skipp)
        :when bindings
          :return
          (let ((name (name pattern)))
            (values (if name
                        (merge-substitutions
                         bindings
                         (make-binding name input :pattern pattern))
                        bindings)
                    pat))))

;; TODO add test (:either (a) (b)): if no sub-pattern match, the
;; iterator should not be updated.
;;
;; TODO add tests with and without skipp
(defmethod match ((pattern either) (iterator iterator) &key skipp)
  "Match an `either' pattern against ITERATOR."
  ;; TODO perf: only copy $before when there's a match
  (let* (($before (copy-iterator iterator))
         ($input (copy-iterator iterator)))
    (loop :for pat :across (patterns pattern)
          :for bindings = (match pat $input :skipp skipp)
          :when bindings
            :do
               (copy-iterator $input iterator)
               (return
                 (let ((name (name pattern)))
                   (values (if name
                               (merge-substitutions
                                bindings
                                (make-binding name $before :pattern pattern))
                               bindings)
                           pat))))))


;;; repetitions

(defmethod match ((pattern repetition) (input vector) &key skipp)
  (match pattern (make-tree-iterator input) :skipp skipp))

;; TODO handler repetition's `maximum'
(defmethod match ((pattern repetition) (iterator iterator) &key skipp)
  (skip iterator skipp)
  ;; Early return: successfully match an empty input.
  (when (and (donep iterator)
             (zerop (minimum pattern)))
    (let ((name (name pattern)))
      (return-from match
        (values (if name
                    (make-binding name
                                  (copy-iterator iterator)
                                  :pattern pattern)
                    t)
                :early-return))))
  (loop
    :with substitution := t
    :with $pattern := (make-pattern-iterator (pattern pattern))
    :with $input := (copy-iterator iterator)
    :with maximum := (maximum pattern)
    :with failed-match-p := nil
    :for $prev-input := (copy-iterator $input) ;; TODO should this be nil?
      :then (copy-iterator $input $prev-input)
    ;; infinite loop guard
    :for i := 1 :then (prog1 (1+ i)
                        (when (<= 1000000 i)
                          (break "Infinite loop? in (match repetition iterator)")))
    :for new-bindings = (match $pattern $input :skipp skipp)
      :then (progn (reset $pattern)
                   (match $pattern $input :skipp skipp))
    :if new-bindings
      :do (let ((merged (merge-substitutions substitution new-bindings)))
            (if merged
                (setf substitution merged)
                ;; conflicting new bindings:
                (setf failed-match-p t)))
    :else
      ;; no new-bindings: failed match
      :do (setf failed-match-p t)
    :until (or failed-match-p
               (donep $input)
               (and maximum
                    #| TODO pretty sure this should be <=, not just < |#
                    (< maximum i))
               ;; TODO (and (not greedyp) (or (<= (minimum pattern) i) <the next pattern matches>)
               )
    :finally
       ;; No more input or, no match
       (when (or
              ;; no more input
              (donep $input)
              ;; no match or conflicting new-bindings
              failed-match-p
              ;; incomplete match
              (not (tree-iterator-done-p $pattern)))
         (return
           (let ((times
                   ;; if it was a match, count the current match,
                   ;; otherwise count only up to the previous one.
                   (if failed-match-p (1- i) i)))
             (when (<= (minimum pattern) times)
               (let (($start (copy-iterator iterator))
                     ($end
                       ;; if it was a match, include the current
                       ;; position, otherwise stop at the previous
                       ;; one.
                       (if failed-match-p $prev-input $input)))
                 ;; update iterator
                 (copy-iterator $end iterator)
                 ;; TODO if max = 1, return a "simple" binding
                 ;; TODO return an object (iterator-range? +
                 ;; binding???) instead of a plist
                 (let ((name (name pattern)))
                   (when name
                     (setf substitution (add-binding substitution
                                                   (make-binding
                                                    name
                                                    ;; bindings
                                                    (list
                                                     ;; :bindings bindings
                                                     :$start $start
                                                     :$end $end
                                                     :times times)
                                                    :pattern pattern)))
                     ;; The new substitution conflicted with the existing ones...
                     (unless substitution
                       (return (values nil :conflicting-binding-with-whole-pattern-substitution))))
                   ;; return the substitution
                   substitution))))))))


;;; Matching standard objects

(defmethod match ((slot-pattern slot-pattern) (instance standard-object) &key)
  (let ((slot-name (slot-name slot-pattern)))
    (if (slot-exists-p instance slot-name)
        (if (slot-boundp instance slot-name)
            (or (match (value slot-pattern) (slot-value instance slot-name))
                (values nil :slot-does-not-match))
            (values nil :slot-not-bound))
        (values nil :slot-does-not-exist))))

;; TODO reuse this to implement and "and" pattern... if I ever need
;; one...
(defmethod match-every ((patterns vector) input)
  (or (zerop (length patterns))
      (loop
        :with substitution := t
        :for pattern :across patterns
        :do (multiple-value-call
               (lambda (new-substitution &rest extra-values)
                 (cond
                   (new-substitution
                    (setf substitution
                          (add-binding substitution new-substitution))
                    (unless substitution
                      (values nil :conflicting-bindings)))
                   (t (return (values nil
                                      pattern
                                      extra-values)))))
             (match pattern input))
        :finally (return substitution))))

(defmethod match ((object-pattern object-pattern)
                  (instance standard-object)
                  &key
                  &aux
                    (class-name (class-name object-pattern))
                    (slots (slots object-pattern)))
  ;; TODO (if (name object-pattern) (make-binding (name
  ;; object-pattern) instance) ...)
  (and
   ;; match the class-name
   (or (null class-name)
       (match class-name (class-name (class-of instance)))
       (return-from match (values nil :different-class-name)))
   ;; match the slots
   (or (null slots)
       (match-every slots instance))))


;;; Matching utilities

(defun extract-names (compiled-pattern)
  (loop
    :with names := (make-hash-table)
    :with it := (make-pattern-iterator compiled-pattern)
    :until (tree-iterator-done-p it)
    :for p := (tree-iterator-value it)
    :when (and (typep p 'pattern) (name p))
      :do (setf (gethash (name p) names) t)
    :do
       (next-preorder it)
    :finally (return (alexandria:hash-table-keys names))))

#++
(progn
  (trace children :methods t)
  (unwind-protect
       (let* ((pattern `((:either (progn (:named ?body (:maybe :_))))))
              (compiled-pattern (compile-pattern pattern)))
         (extract-names compiled-pattern))
    (untrace children)))

#++
(let* ((pattern `((:either (progn (:named ?body (:maybe :?x))))))
        (compiled-pattern (compile-pattern pattern)))
   (extract-names compiled-pattern))

(defmacro let-match ((pattern input
                       &key skipp
                         (input-var (gensym "input"))
                         (substitution (gensym "substitution"))
                         (get-binding (gensym "get-bindings")))
                      &body body)
  (let* ((compiled-pattern (compile-pattern pattern))
         (meta-variables (extract-names compiled-pattern)))
    `(let* ((,input-var ,input)
            (,substitution
              (match ,compiled-pattern ,input-var :skipp ,skipp)))
       (flet ((,get-binding (name)
                (to (find-binding ,substitution name))))
         (declare (ignorable (function ,get-binding)))
         (let ,(loop
                 :for var :in meta-variables
                 :collect `(,var (,get-binding ',var)))
           ,@(loop
               :for var :in meta-variables
               :collect `(declare (ignorable ,var)))
           (progn ,@body))))))
