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


;;; Matching atoms

(defmethod match (pattern input &key &allow-other-keys)
  "Basic \"equal\" matching"
  (equal pattern input))

(defmethod match ((pattern wildcard) input &key &allow-other-keys)
  "Match anything."
  (declare (ignore input))
  t)

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

;; "nil" must not match any other symbols
(defmethod match ((pattern null) (input symbol) &key)
  "Matcho `nil' against another (non-`nil') symbol."
  nil)

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


;;; Matching sequences


;; TODO move to iterators
;; TODO rename to skip-forward
;; TODO add a skip-backward
(declaim (inline skip))
(defun skip ($input skipp)
  ;; TODO support skipp ∈ {t :whitespace :comment)
  (when skipp
   (loop :while (and
                 (not (donep $input))
                 (funcall skipp $input))
         :do (next $input))))

(declaim (inline maybe-stop))
(defun maybe-stop ($input)
  (when (donep $input)
    (throw 'no-match nil)))

(declaim (inline go-down-together))
(defun go-down-together ($pattern $input)
  (let ((sub-pattern (value $pattern)))
    ;; if the sub-pattern is a vector, we
    ;; "recurse" into the sub-pattern
    (when (vectorp sub-pattern)
      (push-subtree $pattern sub-pattern)
      ;; we also recurse into the input data,
      (go-down $input)
      ;; it's possible that there's no data to
      ;; match against in the input's sub-tree
      (maybe-stop $input))))

(defmethod match (($pattern pattern-iterator) (iterator iterator) &key skipp)
  (catch 'no-match
    (loop
      :with bindings = t ;; (make-substitutions)
      :with $input = (copy-iterator iterator)
      :until (or (donep $pattern) (donep $input))
      :for new-bindings = (progn
                            (skip $input skipp)
                            (maybe-stop $input)
                            (go-down-together $pattern $input)
                            (match (value $pattern) $input))
      :do (next $pattern) (next $input)
      :if new-bindings
        ;; collect all the bindings
        :do
           (setf bindings (merge-substitutions bindings new-bindings))
           ;; The new bindings conflicted with the existing ones...
           (unless bindings (return nil))
      :else
        ;; failed to match, bail out of the whole function
        :do (return nil)
      :finally
         (return
           ;; We want to match the whole pattern, but whether we want
           ;; to match the whole input is up to the caller.
           (when (donep $pattern)
             ;; update the interator
             (copy-iterator $input iterator)
             (or bindings t))))))

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
return two values: the match result and the interator on the
input (which can then be used by the caller to verify if all input
where consumed, for example)."
  (let (($pattern (make-pattern-iterator pattern))
        ($input (make-tree-iterator input)))
    (values (match $pattern $input :skipp skipp)
            $input)))


;;; Matching eithers

;; TODO add tests with and without skipp
(defmethod match ((pattern either) input &key skipp)
  "Match an `either' pattern against INPUT."
  (loop :for pat :across (patterns pattern)
        :for bindings = (match pat input :skipp skipp)
        :when bindings
          :return (values bindings pat)))

;; TODO add test (:either (a) (b)): if no sub-pattern match, the
;; iterator should not be updated.
;;
;; TODO add tests with and without skipp
(defmethod match ((pattern either) (iterator iterator) &key skipp)
  "Match an `either' pattern against INTERATOR."
  (let (($input (copy-iterator iterator)))
    (loop :for pat :across (patterns pattern)
          :for bindings = (match pat $input :skipp skipp)
          :when bindings
            :do
               (copy-iterator $input iterator)
               (return (values bindings pat)))))


;;; repetitions

(defmethod match ((pattern repetition) (input vector) &key skipp)
  (match pattern (make-tree-iterator input) :skipp skipp))

;; TODO handler repetition's `maximum'
(defmethod match ((pattern repetition) (iterator iterator) &key skipp)
  (skip iterator skipp)
  ;; Early return: successfully match an empty input.
  (when (and (donep iterator)
             (zerop (minimum pattern)))
    (return-from match t))
  (loop
    :with $pattern := (make-pattern-iterator (pattern pattern))
    :with $input := (copy-iterator iterator)
    :for $prev-input := (copy-iterator $input)
      :then (copy-iterator $input $prev-input)
    ;; TODO remove infinite loop guard
    :for i :from 1 :below 1000
    :for new-bindings = (match $pattern $input :skipp skipp)
      :then (progn (reset $pattern)
                   (match $pattern $input :skipp skipp))
    :when new-bindings
      :collect new-bindings :into bindings
    :do
       ;; No more input or, no match
       (when (or
              ;; no more input
              (donep $input)
              ;; no match
              (not new-bindings)
              ;; incomplete match
              (not (donep $pattern)))
         (return
           (let ((times
                   ;; if it was a match, count the current match,
                   ;; otherwise count only up to the previous one.
                   (if new-bindings i (1- i))))
             (when (<= (minimum pattern) times)
               (let (($start (copy-iterator iterator))
                     ($end
                       ;; if it was a match, include the current
                       ;; position, otherwise stop at the previous
                       ;; one.
                       (if new-bindings $input $prev-input)))
                 ;; update iterator
                 (copy-iterator $end iterator)
                 ;; TODO if max = 1, return a "simple" binding
                 ;; TODO return an object (iterator-range? +
                 ;; binding-sets???) instead of a plist
                 (make-binding
                  ;; TODO don't create a binding (just return t) if
                  ;; the pattern has no name
                  (or (name pattern) pattern)
                  ;; bindings
                  (list
                   :bindings bindings
                   :$start $start
                   :$end $end
                   :times times)))))))))



;;; Convenience automatic coercions

;; TODO delete this and fix everything that breaks 🙃
;; TODO this shouldn't be needed, the (macth iterator iterator) should
;; be able to take care of this.
;;; OOOORRR make this explicit by having another pattern class to represent "going down" >subtree<
(defmethod match ((pattern vector) (input iterator) &key skipp)
  ;; TODO should copy the iterator
  (go-down input)
  (unless (donep input)
    (match (make-pattern-iterator pattern) input :skipp skipp)))
