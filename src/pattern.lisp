;;;; Trying to design a DSL for small refactors

(defpackage #:breeze.pattern
  (:documentation "Pattern matching")
  (:use #:cl #:breeze.iterator #:breeze.generics)
  (:import-from #:breeze.range
                #:range #:range=)
  (:import-from #:breeze.string
                #:symbol-starts-with)
  (:export #:compile-pattern)
  (:export #:pattern
           #:patterns
           #:defpattern
           #:match
           #:wildcard
           #:wildcardp
           #:var
           #:varp
           #:name
           #:sym
           #:sym-package
           #:qualification
           #:wildp
           #:maybe
           #:zero-or-more
           #:repetition
           #:repetitionp
           #:minimum
           #:maximum
           #:maybe
           #:zero-or-more
           #:either
           #:eitherp
           #:pattern-iterator
           #:make-pattern-iterator)
  ;; Working with match results
  (:export #:make-binding
           #:merge-substitutions
           #:find-binding
           #:binding
           #:from
           #:to
           #:substitutions
           #:substitutions-p
           #:merge-sets-of-substitutions
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
;; TODO var with subpattern (? ?x . subpattern)  === (group name pattern)
;; TODO there's no syntax for "repetition", only for "maybe" and "zero-or-more"
;; TODO maybe get some inspiration here: https://docs.racket-lang.org/reference/match.html


;;; Abstract pattern

(defclass pattern ()
  ()
  (:documentation "Abstract pattern"))

(defmethod make-load-form ((s pattern) &optional environment)
  (make-load-form-saving-slots s :environment environment))


;;; Wildcard / don't care

(defclass wildcard ()
  ()
  (:documentation "Pattern that matches anything"))

(defun wildcard ()
  "Make a pattern object that matches anything"
  (load-time-value (make-instance 'wildcard)))

(defun wildcardp (x)
  "Is X an object of class `wildcard'?"
  (eq (class-name (class-of x)) 'wildcard))

(defmethod eqv ((a wildcard) (b wildcard))
  t)

(defmethod make-load-form ((s wildcard) &optional environment)
  (make-load-form-saving-slots s :environment environment))


;;; Vars

(defclass var (pattern)
  ((name
    :initform nil
    :initarg :name
    :accessor name
    :documentation "The name of the var.")
  (pattern
    :initform nil
    :initarg :pattern
    :accessor pattern
    :documentation "An optional pattern to match against."))
  (:documentation "Pattern that creates a binding."))

(defun var (name &optional (pattern nil patternp))
  "Make a pattern object that creates a binding."
  (make-instance 'var
                 :name name
                 :pattern (if patternp
                            pattern
                            (wildcard))))

(defun varp (x)
  "Is X an object of class `var'?"
  (eq (class-name (class-of x)) 'var))

(defmethod print-object ((var var) stream)
  "Print an object of type `var'."
  (let ((*print-case* :downcase)
        (*print-readably* nil))
    (format stream "(var ~s~:[ ~s~;~])"
            (name var)
            (wildcardp (pattern var))
            (pattern var))))

(defmethod eqv ((a var) (b var))
  "Test that A and B are both object of type `var' with the same name."
  (and (varp a)
       (varp b)
       (eq (name a) (name b))
       (eqv (pattern a) (pattern b))))

(defmethod make-load-form ((s var) &optional environment)
  (make-load-form-saving-slots s :environment environment))


;;; Symbol patterns

#|

A pattern type to match symbols in packages that are not defined in
the current image.

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
        (*print-readably* nil))
    (flet ((pp (x)
             (cond
               ((packagep x)
                (format nil "(find-package '#:~a)"
                        (package-name x)))
               ((keywordp x) (prin1-to-string x))
               ((symbolp x) (format nil "'~s" x))
               (t (prin1-to-string x)))))
      (format stream "(sym ~a ~a~:[~; ~s~])"
              (pp (sym-package sym))
              (pp (name sym))
              (not (eq :wild (qualification sym)))
              (qualification sym)))))

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

(defmethod eqv ((a sym) (b sym))
  (with-slots ((pak-a package) (name-a name) (qual-a qualification)) a
    (with-slots ((pak-b package) (name-b name) (qual-b qualification)) b
      (and
       ;; Compare package "pattern"
       (or
        ;; compare by eq, in case they're both actual package objects,
        ;; not just designators
        (eq pak-a pak-b)
        ;; N.B. : case-insensitive comparison
        (cond
          ((packagep pak-a)
           (string-equal (package-name pak-a) pak-b))
          ((packagep pak-b)
           (string-equal (package-name pak-b) pak-a))
          (t
           (string-equal pak-a pak-b))))
       (or (eq name-a name-b)
           (string-equal name-a name-b))
       (eq qual-a qual-b)))))

(defmethod make-load-form ((s sym) &optional environment)
  (make-load-form-saving-slots s :environment environment))


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

(defmethod eqv ((a repetition) (b repetition))
  "Test that A and B are both object of type `repetition' with the same
limits."
  (and (repetitionp a)
       (repetitionp b)
       (eqv (pattern a) (pattern b))
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

(defmethod make-load-form ((s repetition) &optional environment)
  (make-load-form-saving-slots s :environment environment))

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
  ;; pattern, perhaps even check that they are not `eqv'.
  (make-instance 'either :patterns patterns))

(defun eitherp (x)
  "Is X an object of class `either'?"
  (eq (class-name (class-of x)) 'either))

(defmethod eqv ((a either) (b either))
  "Test that A and B are both object of type `either' with the same
subpatterns."
  (and (eitherp a)
       (eitherp b)
       ;; This works because patterns are vectors...
       (eqv (patterns a)
            (patterns b))))

(defmethod print-object ((either either) stream)
  "Print an object of type `either'."
  (print-unreadable-object
      (either stream :type t :identity t)
    ;; TODO it could be nice to print very short parts of the sub
    ;; patterns, or all of them if they're very small.
    (format stream "~s" (length (patterns either)))))

(defmethod make-load-form ((s either) &optional environment)
  (make-load-form-saving-slots s :environment environment))


;;; Pattern compilation from lists and symbols to vectors and structs

(defun var-symbol-p (x)
  "Does the symbol X represents a \"var\" pattern?"
  (symbol-starts-with x #\?))

(defun wildcard-symbol-p (x)
  "Does the symbol X represents an \"wildcard\" pattern?"
  (symbol-starts-with x #\_))

(declaim (inline wildp))
(defun wildp (x)
  (eq x :wild))

#|

Originally, I took the design decision of using the pattern objects
themselves to serve as the "identity" of a binding. It's easier to
explain with an example:

the pattern ?x compiles to #1=(var ?x) — an object of type 'var with
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
same 'var pattern object when compiling ?name.

"rewrites" use case:

let's say you matched a pattern against a form and you want to extract
the values and generate a new form using them, you can use a second
pattern and sustitute the vars in it with the values from matching
the first pattern.

pattern A: (+ ?x (- ?y))
pattern B: (- ?x ?y)

but, again, without sharing the pattern objects for vars, one would
need to associate each vars from each patterns using the vars'
names (that implies walking the patterns to find the vars).

---

I ditched the concept of re-usable pre-defined patterns (for now). I
also think that my original motivation of avoiding conflicts was a bit
too much (especially that the names are symbols, they can be from any
packages...).

With these requirements gone, I think it would simplify a lot of code
if we were using the name of the vars in the bindings. That means
that we wouldn't need the *var-pool* at all anymore. (we don't even
need to de-duplicate pattern objects whithin one pattern.)

|#

(defun compile-pattern (pattern)
  "Compiles a PATTERN (specified as a list)"
  (%compile-pattern pattern))

;; Default: leave as-is
(defmethod %compile-pattern (pattern) pattern)

;; Compile symbols
(defmethod %compile-pattern ((pattern symbol))
  (cond
    ((var-symbol-p pattern) (var pattern))
    ((wildcard-symbol-p pattern) (wildcard))
    (t pattern)))

;; Compile lists
(defmethod %compile-pattern ((pattern cons))
  ;; Dispatch to another method that is eql-specialized on the first
  ;; element of the list.
  (compile-compound-pattern (first pattern) pattern))

;; Default list compilation: recurse and convert to vector.
(defmethod compile-compound-pattern (token pattern)
  (map 'vector #'%compile-pattern pattern))

(defmethod compile-compound-pattern ((token (eql :maybe)) pattern)
  "Compile (:maybe ...)"
  ;; TODO check the length of "pattern"
  (maybe (%compile-pattern (second pattern))))

(defmethod compile-compound-pattern ((token (eql :var)) pattern)
  "Compile (:var name [pattern])"
  (destructuring-bind (name sub-pattern)
      (rest pattern)
    (var name sub-pattern)))

(defmethod compile-compound-pattern ((token (eql :zero-or-more)) pattern)
  "Compile (:zero-or-more ...)"
  (zero-or-more (%compile-pattern (rest pattern))))

(defmethod compile-compound-pattern ((token (eql :either)) patterns)
  "Compile (:either ...)"
  (either (%compile-pattern (rest patterns))))

(defmethod compile-compound-pattern ((token (eql :symbol)) pattern)
  "Compile (:symbol ...)"
  (destructuring-bind (symbol-name-pattern
                       &optional
                         (package-name-pattern :wild)
                         (qualification-pattern :wild))
      (rest pattern)
    (sym package-name-pattern
         symbol-name-pattern
         qualification-pattern)))


;;; Bindings (e.g. the result of a successful match)

(defclass binding ()
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to)
   ;; TODO source/location: optional slot to register where this
   ;; information came from. This would come in handy for error
   ;; messages; especially if we try to use the paaterns for
   ;; unification, and use unification for type checking.
   )
  (:documentation "A binding"))

(defun make-binding (from to)
  (make-instance 'binding :from from :to to))

(defun bindingp (x)
  (typep x 'binding))

(defmethod print-object ((binding binding) stream)
  (print-unreadable-object
      (binding stream :type t)
    (format stream "~s → ~s" (from binding) (to binding))))

(defmethod to ((_ null)))
(defmethod from ((_ null)))

(defmethod eqv ((a binding) (b binding))
  (and (eqv (from a) (from b))
       (eqv (to a) (to b))))

(defclass substitutions ()
  ((bindings
    :initform (make-hash-table)
    :initarg :bindings
    :reader bindings))
  ;; TODO add a union-hash to detect cycles...
  (:documentation "A set of bindings"))

(defun substitutions-p (x)
  (or (eq x t)
      (typep x 'substitutions)))

(defmethod print-object ((substitutions substitutions) stream)
  (print-unreadable-object
      (substitutions stream :type t)
    (let* ((bindings (bindings substitutions))
           (size (hash-table-count bindings)))
      (cond
        ((zerop size) (write-string "(empty)" stream))
        ((= 1 size) (prin1 (alexandria:hash-table-alist bindings) stream))
        (t (format stream "(~d bindings)" size))))))

(defmethod eqv ((a substitutions) (b substitutions))
  (eqv (bindings a) (bindings b)))

(defmethod emptyp ((substitutions substitutions))
  (zerop (hash-table-count (bindings substitutions))))

;; I think using the convention of "T represents an empty binding set,
;; which represents a successful match without captures" is nice, the
;; GC might like it too...
(defmethod emptyp ((substitutions (eql t)))
  t)

(defmethod emptyp ((binding binding))
  nil)

(defun make-substitutions (&key bindings)
  (if bindings
    (make-instance 'substitutions :bindings bindings)
    (make-instance 'substitutions)))

(defun copy-substitutions (substitutions)
  (make-substitutions
   :bindings (alexandria:copy-hash-table (bindings substitutions))))

(defun find-binding (substitutions from)
  (gethash from (bindings substitutions)))

;; TODO maybe this could be a method instead of a defun?
(defun set-binding (substitutions binding)
  (setf (gethash (from binding) (bindings substitutions)) binding))

(defmethod add-binding ((substitutions substitutions) (_ (eql t)))
  ;; nothing to do
  t ; success
  )

(defmethod add-binding ((substitutions substitutions) (_ (eql nil)))
  ;; nothing to do
  nil ; failure
  )

(defmethod add-binding ((substitutions substitutions) (new-binding binding))
  (let ((old-binding (find-binding substitutions (from new-binding))))
    (and (if old-binding
             ;; (error "Conflicting bindings: ~a ~a" a b)
             (eql (to old-binding) (to new-binding))
             (set-binding substitutions new-binding))
         substitutions)))

(defun ensure-substitutions (x)
  (etypecase x
    (substitutions x)
    (binding (let ((substitutions (make-substitutions)))
               (add-binding substitutions x)
               substitutions))
    (null nil)
    ((eql t) t)))

(defun merge-substitutions (bindings1 bindings2)
  (cond
    ((or (null bindings1) (null bindings2)) nil)
    ((and (eq t bindings1) (eq t bindings2)) t)
    ;; when merging two binding instances instead of substitutionss
    ((and (bindingp bindings1) (bindingp bindings2))
     (let ((result (make-substitutions)))
       (add-binding result bindings1)
       (when (add-binding result bindings2)
         result)))
    ((emptyp bindings1) (ensure-substitutions bindings2))
    ((emptyp bindings2) (ensure-substitutions bindings1))
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
     ;; the substitutions.
     ;;
     ;; TODO this might be faster if it merged the common bindings first, but I'm not sure how many bindings must there be to be worth it.
     ;;
     ;; TODO another perf heuristic: copy the smallest or biggest
     ;; substitutions. copying the smallest would be faster, but it
     ;; implies that `add-bindings'will be called more times.
     (let ((result (copy-substitutions bindings1)))
       (loop :for from2 :being :the :hash-key :of (bindings bindings2) :using (hash-value binding2)
             :for successp = (add-binding result binding2)
             :unless successp :do (return))
       result))))

(defun merge-sets-of-substitutions (set-of-substitutions1 set-of-substitutions2)
  "Merge two set (list) of `substitutions', returns a new set of
`substitutions'.

Matching a pattern against a set of values (e.g. an egraph) will yield
a set of independent substitutions. During the matching process, we
might need to refine the \"current\" set of \"partial\"
`substitutions'.  This is done by computing the Cartesian product of
the two sets of substitutions and keeping only the resulting
substitutions that don't have any conflicting bindings."
  (loop
    :for substitutions1 :in set-of-substitutions1
    :append (loop
              :for substitutions2 :in set-of-substitutions2
              :for merged-substitutions = (merge-substitutions
                                      substitutions1 substitutions2)
              :when merged-substitutions
                :collect merged-substitutions)))


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

(defmethod match ((pattern var) input &key)
  "Match a var (create a binding)"
  (make-binding (name pattern) input))

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

;; TODO could it be useful to support `var' in the `sym' 's name,
;; package and qualification slots?
;; TODO Or maybe just (optionaly) create some binidings here...
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

(defmethod match ((pattern var) ($input iterator) &key skipp)
  "Match a `var' pattern against the current value of an
`iterator'. This always match, it advances the iterator and returns a
binding."
  (skip $input skipp)
  (unless (donep $input)
    (let ((binding-to (copy-iterator $input)))
      ;; consume one element of the input
      (next $input)
      (make-binding (name pattern) binding-to))))

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
          :return (values bindings pat)))


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
  (cond
    ((and pattern (null bindings)) pattern)
    ((null pattern) nil)
    ((and pattern bindings)
     (check-type pattern atom)
     (check-type bindings (or binding substitutions (eql t)))
     (flet ((substitute1 (x)
              (etypecase x
                (var
                 (alexandria:if-let ((binding (find-binding bindings (name x))))
                   (to binding)
                   ;; TODO this could signal a condition (binding not
                   ;; found)
                   x))
                ((or symbol number) x))))
       (if (vectorp pattern)
           (map result-type
                ;; Note: we could've recurse directly into
                ;; pattern-subtitute, but not doing so make tracing
                ;; (and debugging) tremenduously easier.
                #'(lambda (subpattern)
                    (if (vectorp subpattern)
                        (pattern-substitute subpattern bindings result-type)
                        (substitute1 subpattern)))
                pattern)
           (substitute1 pattern))))))



;;; Rules and rewrites


;; TODO "rules" would be "bidirectional" and "rewrites" wouldn't.
;; TODO (defun rule (a b) ...)
;; TODO (defun make-rewrite (antecedent consequent) ...)

#++ (progn
      (defclass abstract-rule () ())

      (defclass rule (abstract-rule) ())

      (defun make-rule (a b)
        (list :rule
              (compile-pattern a)
              (compile-pattern b)))

      (defun make-rewrite (a b)
        (list :rewrite
              (compile-pattern a)
              (compile-pattern b))))

(defun make-rewrite (pattern template)
  (cons ;; TODO use a class instead
   (compile-pattern pattern)
   (compile-pattern template)))

(defun rewrite-pattern (rewrite)
  "Get the pattern of a REWRITE rule."
  (car rewrite))

(defun rewrite-template (rewrite)
  "Get the template of a REWRITE rule."
  (cdr rewrite))
