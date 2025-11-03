;;;; Trying to design a DSL for small refactors

(defpackage #:breeze.pattern
  (:documentation "Pattern matching")
  (:use #:cl #:breeze.iterator #:breeze.generics)
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

;; TODO guard (? pred pat) => match pattern, then apply pred
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
    :documentation "An optional pattern to match against.")
  (multi-valued-p
    :initform nil
    :initarg :multi-valued-p
    :accessor multi-valued-p
    :documentation "Whether this pattern variables creates new binding every time there is
a match for a variable of the same name."))
  (:documentation "Pattern that creates a binding."))

(defun var (name &optional (pattern nil patternp) &key multi-valued-p)
  "Make a pattern object that creates a binding."
  (make-instance 'var
                 :name name
                 :pattern (if patternp
                            pattern
                            (wildcard))
                 :multi-valued-p multi-valued-p))

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
