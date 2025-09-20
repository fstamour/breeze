;;;; Trying to design a DSL for small refactors

(defpackage #:breeze.pattern
  (:documentation "Pattern matching")
  (:use #:cl #:breeze.iterator)
  (:import-from #:breeze.generics
                #:name)
  (:import-from #:breeze.string
                #:symbol-starts-with)
  (:export #:compile-pattern)
  (:export #:pattern
           #:patterns
           #:defpattern
           #:match
           #:wildcard
           #:wildcardp
           #:term
           #:termp
           #:term=
           #:name
           #:sym
           #:sym-package
           #:qualification
           #:maybe
           #:zero-or-more
           #:repetition
           #:repetitionp
           #:repetition=
           #:minimum
           #:maximum
           #:maybe
           #:zero-or-more
           #:either
           #:eitherp
           #:either=
           #:pattern=
           #:pattern-iterator
           #:make-pattern-iterator)
  ;; Working with match results
  (:export #:make-binding
           #:merge-sets-of-bindings
           #:find-binding
           #:binding
           #:from
           #:to
           #:binding-set
           #:binding-set-p
           #:merge-bindings
           #:pattern-substitute)
  (:export #:make-rewrite
           #:rewrite-pattern
           #:rewrite-template))

(in-package #:breeze.pattern)

;; TODO find-match
;; TODO find-partial-match
;; TODO fuzzy match (e.g. symbol with a low edit distance is considerded a match)

;; TODO callbacks? instead of bindings??
;; TODO guard (? pred pat) => match pattern, then apply pred
;; TODO term with subpattern (? ?x . subpattern)  === (group name patterm)
;; TODO there's no syntax for "repetition", only for "maybe" and "zero-or-more"
;; TODO maybe get some inspiration here: https://docs.racket-lang.org/reference/match.html


;;; Abstract pattern

(defclass pattern ()
  ()
  (:documentation "Abstract pattern"))


;;; Wildcard / don't care

;; TODO could be useful to give this a name anyway, for debugging?
(defclass wildcard ()
  ()
  (:documentation "Pattern that matches anything"))

(defun wildcard ()
  "Make a pattern object that matches anything"
  (make-instance 'wildcard))

(defun wildcardp (x)
  "Is X an object of class `wildcard'?"
  (eq (class-name (class-of x)) 'wildcard))


;;; Terms

;; Decision: I chose "term" and not "variable" to avoid clashes with
;; cl:variable
(defclass term (pattern)
  ((name
    :initform nil
    :initarg :name
    :accessor name
    :documentation "The name of the term."))
  (:documentation "Pattern that matches anything and creates a binding."))

(defun term (name)
  "Make a pattern object that matches anything and create a binding."
  (make-instance 'term :name name))

(defun termp (x)
  "Is X an object of class `term'?"
  (eq (class-name (class-of x)) 'term))

(defmethod print-object ((term term) stream)
  "Print an object of type `term'."
  (print-unreadable-object
      (term stream :type t :identity t)
    (format stream "~s" (name term))))

;; TODO this could be a method on eqv
(defun term= (a b)
  "Test that A and B are both object of type `term' with the same name."
  (and (termp a)
       (termp b)
       (eq (name a)
           (name b))))

;;; Symbol patterns

#|

TODO add a special pattern type to match symbols in packages that are
not defined in the current image.

What do I want it to look like?

- should probably be "compiled"
  - (compile-pattern (sym ...))

* normal/simple match: "cl:null"
  - should match any nicknames
  - should match any case
  - should match current-package-symbol, qualified-symbol and possibly-internal-symbol

* exact match: "^cl:null$"
  - (eq cl) (eq null)
  - case-insensitive: (sym '(equal cl) (equal null))
    - default

* exact package name:
  - "^cl$:null" (= cl) null

* any package
  - '(:any "null")

* any symbol
  - '(cl :any)

* qualification: nil, current, internal, :any, :or <===

|#

(defclass sym ()
  ((name
    :initform nil
    :initarg :name
    :accessor name
    :documentation "The name of the symbol.")
  (package
    :initform nil
    :initarg :package
    :accessor sym-package
    :documentation "The name of the symbol's package.")
  (qualification
    :initform nil
    :initarg :qualification
    :accessor qualification
    :documentation "How many #\\: is there in the symbol"))
  (:documentation "An object representing a symbol. Meant to be use for matching against
symbol designators without having to define packages or intern
symbols."))

(defmethod print-object ((sym sym) stream)
  (let ((*print-case* :downcase)
        (*print-readably* t))
    (format stream "(sym ~:[~;'~]~s ~:[~;'~]~s~:[~; ~s~])"
            (symbolp (sym-package sym)) (sym-package sym)
            (symbolp (name sym)) (name sym)
            (not (eq :wild (qualification sym)))
            (qualification sym))))

(defun sym (package name &optional (qualification :wild))
  ;; TODO validation
  ;; :wild means "don't care"
  ;; nil means "uninterned"
  ;; (check-type package (or string (member :wild nil :keyword)))
  ;; (check-type name (or string (member :wild)))
  ;; (check-type qualification (member :wild :current :internal :uninterned))
  (make-instance 'sym
                 :package package
                 :name name
                 :qualification qualification))


;;; Repetitions

(defclass repetition (pattern)
  ((pattern
    :initform nil
    :initarg :pattern
    :accessor pattern
    :documentation "The pattern to be match repeatedly.")
   (minimum
    :initform nil
    :initarg :minimum
    :accessor minimum
    :documentation "The minimum number of times the pattern must match to be considered a
successful match.")
   (maximum
    :initform nil
    :initarg :maximum
    :accessor maximum
    :documentation "The maximum number of times the pattern must match to be considered a
successful match.")
   ;; TODO greedyp
   )
  (:documentation "A repeated pattern."))

(defun repetition (pattern &optional (min 0) max)
  "Make a pattern object that matches a pattern repeatedly."
  (make-instance 'repetition
                 :pattern pattern
                 :minimum min
                 :maximum max))

(defun repetitionp (x)
  "Is X an object of class `repetition'?"
  (eq (class-name (class-of x)) 'repetition))

;; TODO this could be a method on eqv
(defun repetition= (a b)
  "Test that A and B are both object of type `repetition' with the same
limits."
  (and (repetitionp a)
       (repetitionp b)
       (pattern= (pattern a) (pattern b))
       (= (minimum a) (minimum b))
       (let ((ma (maximum a))
             (mb (maximum a)))
         (or (eq ma mb)
             (and (numberp ma) (numberp mb)
                  (= ma mb))))))

(defmethod print-object ((repetition repetition) stream)
  "Print an object of type `repetition'."
  (print-unreadable-object
      (repetition stream :type t :identity t)
    (format stream "[~s-~s]"
            (minimum repetition)
            (maximum repetition))))

(defun maybe (pattern)
  "Make a pattern object that optionally matches a pattern once."
  (repetition pattern 0 1))

(defun zero-or-more (pattern)
  "Make a pattern object that optionally matches a pattern many times."
  (repetition pattern 0 nil))


;;; Eithers

(defclass either (pattern)
  ((patterns
    :initform nil
    :initarg :patterns
    :accessor patterns
    :documentation "The sequence of patterns to be tried one after the other."))
  (:documentation "An ordered set of patterns."))

(defun either (patterns)
  "Make a pattern object that must match one of its subpatterns."
  (check-type patterns vector)
  ;; maybe we would also like to check that there's more than 1
  ;; pattern, perhaps even check that they are not `pattern='.
  (make-instance 'either :patterns patterns))

(defun eitherp (x)
  "Is X an object of class `either'?"
  (eq (class-name (class-of x)) 'either))

;; TODO this could be a method on eqv
(defun either= (a b)
  "Test that A and B are both object of type `either' with the same
subpatterns."
  (and (eitherp a)
       (eitherp b)
       ;; This works because patterns are vectors...
       (pattern= (patterns a)
                 (patterns b))))

(defmethod print-object ((either either) stream)
  "Print an object of type `either'."
  (print-unreadable-object
      (either stream :type t :identity t)
    ;; TODO it could be nice to print very short parts of the sub
    ;; patterns, or all of them if they're very small.
    (format stream "~s" (length (patterns either)))))


;;; Pattern comparison

;; TODO this could be a method on eqv
(defgeneric pattern= (a b)
  (:documentation "Test if two patterns are the same. (Note that this doesn't test for
equivalence, just for equality."))

(defmethod pattern= (a b)
  (equal a b))

(defmethod pattern= ((a wildcard) (b wildcard))
  t)

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
  (def term)
  (def repetition)
  (def either))


;;; Pattern compilation from lists and symbols to vectors and structs

(defun term-symbol-p (x)
  "Does the symbol X represents a \"term\" pattern?"
  (symbol-starts-with x #\?))

(defun wildcard-symbol-p (x)
  "Does the symbol X represents an \"wildcard\" pattern?"
  (symbol-starts-with x #\_))

(declaim (inline wildp))
(defun wildp (x)
  (eq x :wild))

#|

TODO get rid of *term-pool* and use the term's names when creating
bindings.

Explanations/rationale:

Originally, I took the design decision of using the pattern objects
themselves to serve as the "identity" of a binding. It's easier to
explain with an example:

the pattern ?x compiles to #1=(term ?x) — an object of type 'term with
the name (slot) ?x

when matched against, for example, 42, it would create the
binding (#1# . 42) not (?x . 42) — the key is the pattern object, not
its name

so when a pattern results in a lot of bindings, you would search for
the pattern object, not by its name.

The motivation behind that design was that if you have a lot of
pre-defined patterns, you might want to avoid name collisions, and the
chances that you re-use a pattern object between two different
patterns should be pretty low.

But what if you _do_ want to have two patterns that share the
same "pattern variable"?

There are 2 main use cases for wanting to share pattern objects
between multiple patterns, matching with back-references and rewrites.

"back-references" use case:

let's say you have 2 pre-defined patterns: "(defun ?name ...)"
and "(funcall '?name ...)", it's possible that one would like to
find "defuns with funcalls to itself in its body"... It's more
efficient for the pattern matching algorithm to take care of that than
the user checking if both ?names in both patterns are the same. But in
order to do that, you need for both of these patterns to re-use the
same 'term pattern object when compiling ?name.

"rewrites" use case:

let's say you matched a pattern against a form and you want to extract
the values and generate a new form using them, you can use a second
pattern and sustitute the terms in it with the values from matching
the first pattern.

pattern A: (+ ?x (- ?y))
pattern B: (- ?x ?y)

but, again, without sharing the pattern objects for terms, one would
need to associate each terms from each patterns using the terms'
names (that implies walking the patterns to find the terms).

---

I ditched the concept of re-usable pre-defined patterns (for now). I
also think that my original motivation of avoiding conflicts was a bit
too much (especially that the names are symbols, they can be from any
packages...).

With these requirements gone, I think it would simplify a lot of code
if we were using the name of the terms in the bindings. That means
that we wouldn't need the *term-pool* at all anymore. (we don't even
need to de-duplicate pattern objects whithin one pattern.)

|#
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
    ((wildcard-symbol-p pattern)
     ;; TODO (optimization) this creates a new "wildcard" object everytime,
     ;; which is not really necessary.
     (wildcard))
    (t pattern)))

;; Compile lists
(defmethod %compile-pattern ((pattern cons))
  ;; Dispatch to another method that is eql-specialized on the first
  ;; element of the list.
  (compile-compound-pattern (first pattern) pattern))

;; Default list compilation: recurse and convert to vector.
(defmethod compile-compound-pattern (token pattern)
  (map 'vector #'%compile-pattern pattern))

;; Compile (:maybe ...)
(defmethod compile-compound-pattern ((token (eql :maybe)) pattern)
  ;; TODO check the length of "pattern"
  (maybe (%compile-pattern (second pattern))))

;; Compile (:zero-or-more ...)
(defmethod compile-compound-pattern ((token (eql :zero-or-more)) pattern)
  (zero-or-more (%compile-pattern (rest pattern))))

;; Compile (:either ...)
(defmethod compile-compound-pattern ((token (eql :either)) patterns)
  (either (%compile-pattern (rest patterns))))


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

(defmethod to ((_ null)))
(defmethod from ((_ null)))

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
        ((zerop size) (write-string "(empty)" stream))
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
a set of independent bindings. During the matching process, we might
need to refine the \"current\" set of bindings. Long-story short, this
is analogous to computing the Cartesian product of the two sets of
bindings and keeping only those that have not conflicting bindings."
  (loop :for bindings1 :in set-of-bindings1
        :append (loop :for bindings2 :in set-of-bindings2
                      :for merged-bindings = (merge-bindings
                                              bindings1 bindings2)
                      :when merged-bindings
                        :collect merged-bindings)))


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

(defmethod match ((pattern term) input &key)
  "Match a term (create a binding)"
  (make-binding pattern input))

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

;; TODO could it be useful to support `term' in the `sym' 's name,
;; package and qualification slots?
;; TODO Or maybe just (optionaly) create some binidings here...
;; TODO regex???
(defmethod match ((pattern sym) (symbol symbol) &key)
  "Match a pattern of type `sym' against a symbol."
  (with-slots ((symbol-name-pattern name)
               (package-name-pattern package))
      pattern
    (let* ((package (symbol-package symbol))
           (package-name (and package (package-name package)))
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
      :with bindings = t ;; (make-binding-set)
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
           (setf bindings (merge-bindings bindings new-bindings))
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

(defmethod match ((pattern term) ($input iterator) &key skipp)
  "Match a `term' pattern against the current value of an
`iterator'. This always match, it advances the iterator and returns a
binding."
  (skip $input skipp)
  (unless (donep $input)
    (let ((binding-to (copy-iterator $input)))
      ;; consume one element of the input
      (next $input)
      (make-binding pattern binding-to))))

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
;; TODO input should be an iterator...
(defmethod match ((pattern either) input &key skipp)
  "Match an `either' pattern against INPUT."
  (loop :for pat :across (patterns pattern)
        :for bindings = (match pat input :skipp skipp)
        :when bindings
          ;; TODO this is probably very wrong
          :return (make-binding pattern
                                (if (eq t bindings)
                                    input
                                    bindings))))


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
                 ;; TODO return an object (iterator-range? +
                 ;; binding-sets???) instead of a plist
                 (make-binding
                  pattern
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
;; TODO (defun make-rewrite (antecedent consequent) ...)

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
