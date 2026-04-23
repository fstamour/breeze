;; TODO rename this whole file "parse tree matching"

(uiop:define-package #:breeze.analysis
    (:documentation "Pattern matching against parse trees")
  (:use #:cl)
  (:use-reexport #:breeze.iterator
                 #:breeze.generics
                 #:breeze.parser
                 #:breeze.pattern)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:export #:parse-symbol-node)
  ;; Tree/Form predicate
  (:export #:with-match
           #:define-node-matcher
           #:quotedp
           #:number-node-p)
  ;; Enclosing form navigation
  (:export #:normalize-pattern-predicate ; TODO move to the previous section
           #:find-enclosing-node
           #:find-nearest-enclosing-node
           #:find-furthest-enclosing-node
           #:find-all-enclosing-nodes))

(in-package #:breeze.analysis)


;;; Integrating pattern.lisp and parser.lisp

(defun match-parser-state (pattern state &key skipp)
  (match pattern (make-node-iterator state) :skipp skipp))

(defmethod match (pattern (state state) &key skipp)
  (match-parser-state pattern state :skipp skipp))

(defmethod match ((pattern null) (state state) &key skipp)
  "Nil should not match a parse-tree (use an empty vector as pattern to
match an empty parse tree."
  (declare (ignore skipp))
  nil)

(defmethod match ((pattern symbol) (state state) &key skipp)
  (declare (ignore skipp))
  ;; These should return nil because we're trying to match 1 symbol
  ;; against a list of nodes (even if that list is empty).
  nil)

(defun same-package-p (package-designator1 package-designator2)
  "Check if both package-designators designates the same package.
The designators can be strings, symbols or packages."
  (or
   (eq package-designator1 package-designator2)
   (and
    (not (packagep package-designator1))
    (not (packagep package-designator2))
    (string= package-designator1 package-designator2))
   (when-let ((p1 (find-package package-designator1))
                         (p2 (find-package package-designator2)))
     (eq p1 p2))))

(defun same-symbol-p (symbol-name package-designator1 package-designator2)
  "Check if a symbol-name designate the same symbol in two different packages."
  (or (same-package-p package-designator1 package-designator2)
      (when-let ((p1 (find-package package-designator1))
                            (p2 (find-package package-designator2)))
        (let ((s1 (find-symbol symbol-name p1))
              (s2 (find-symbol symbol-name p2)))
          ;; this makes sure we don't compare s1 and s2 if they are
          ;; nil, unless NIL was the symbol we were looking for.
          (and (string= symbol-name s1)
               (string= symbol-name s2)
               (eq s1 s2))))))

(defun match-symbol (symbol-name-pattern symbol-name)
  (or (wildp symbol-name-pattern)
      (string= symbol-name-pattern symbol-name)))

(defun match-qualification (qualification-pattern qualification)
  (or (wildp qualification-pattern)
      (eq qualification-pattern qualification)))

;; TODO (trivial-package-local-nicknames:package-local-nicknames)
(defun match-package (package-name-pattern package-name qualification)
  (cond
    ((wildp package-name-pattern) t)
    ((eq :uninterned qualification)
     (null package-name-pattern))
    ((null package-name-pattern) nil)
    ;; if the package pattern is "keyword"
    ;; TODO keyword-package-p
    ((string= :keyword (or (and (packagep package-name-pattern)
                                (package-name package-name-pattern))
                           package-name-pattern))
     (or (eq :keyword qualification)
         (and (eq :possibly-internal qualification)
              (string= :keyword package-name))))
    ;; package:name or package::name
    ((member qualification '(:qualified :possibly-internal))
     (same-package-p package-name package-name-pattern))
    ((eq qualification :current)
     ;; TODO check the current-package
     t)))

(defmethod match ((pattern sym) ($node node-iterator) &key skipp)
  "Match a `sym' pattern against a string."
  ;; TODO export skip...
  (breeze.pattern::skip $node skipp)
  (with-slots ((symbol-name-pattern name)
               (package-name-pattern package)
               (qualification-pattern qualification))
      pattern
    (when-let ((parsed-symbol (parse-symbol-node $node)))
      (destructuring-bind (qualification symbol-name &optional package-name)
          parsed-symbol
        (and (match-symbol symbol-name-pattern symbol-name)
             (match-qualification qualification-pattern qualification)
             ;; package:name or package::name
             (if (and
                  (not (wildp symbol-name-pattern))
                  (not (wildp package-name-pattern))
                  (member qualification '(:qualified :possibly-internal)))
                 (same-symbol-p symbol-name
                                package-name package-name-pattern)
                 (match-package package-name-pattern package-name
                                qualification)))))))


(defun match-symbol-to-token (symbol $node)
  "Match a symbol against a parse tree's node."
  (check-type $node node-iterator)
  (check-type symbol symbol)
  (match (sym (symbol-package symbol)
              (symbol-name symbol))
    $node))

(defmethod match ((pattern symbol) (node-iterator node-iterator) &key skipp)
  "Match a symbol against a parse tree's node."
  (breeze.pattern::skip node-iterator skipp)
  (match-symbol-to-token pattern node-iterator))

(defmethod match ((pattern null) (node-iterator node-iterator) &key skipp)
  "Match the symbol `nil' against a parse tree's node."
  (breeze.pattern::skip node-iterator skipp)
  (match-symbol-to-token pattern node-iterator))

(defmethod match ((pattern var) (state state) &key skipp)
  "Match a var against a parse state."
  (match-parser-state pattern state :skipp skipp))

(defmethod match ((pattern simple-var) (state state) &key skipp)
  "Match a var against a parse state."
  (match-parser-state pattern state :skipp skipp))

;; TODO One method per type of node
;; Update: I recently added "object-patterns"
#++
(progn
  whitespace
  block-comment
  line-comment
  token
  parens
  punctuation
  string
  quote
  quasiquote
  dot
  comma
  sharp
  sharp-char
  sharp-function
  sharp-vector
  sharp-bitvector
  sharp-uninterned
  sharp-eval
  sharp-binary
  sharp-octal
  sharp-hexa
  sharp-complex
  sharp-structure
  sharp-pathname
  sharp-feature
  sharp-feature-not
  sharp-radix
  sharp-array
  sharp-label
  sharp-reference
  sharp-unknown)


;;; Syntax tree inspection, matching, etc

;; TODO match-case

;; TODO rename to with-match-node? or let-match-node
;; TODO swap the argument's order to match (ha!) let-match and match
(defmacro with-match ((node-iterator pattern) &body body)
  `(let-match (,pattern ,node-iterator
               :skipp #'whitespace-or-comment-node-p)
     ,@body))

;; TODO be able to name the "node-iterator" argument
;; TODO maybe, be able to do some checks _before_ trying to match
;; TODO maybe, add some options for common stuff ... e.g. "don't match if quoted"
(defmacro define-node-matcher (name (pattern) &body body)
  `(defun ,name (node-iterator)
     ,(format nil "Does NODE-ITERATOR match ~s?" pattern)
     (declare (ignorable node-iterator))
     (with-match (node-iterator ,pattern)
       ,@body)))


;;; Quoting

;; TODO this takes care of the ` reader-macro, but not the (quote ...) special operator :/
;; TODO nor can if figure out if part of a macro is evaluated or not
(defun quotedness (node-iterator)
  (let ((depth (slot-value node-iterator 'depth)))
    (loop :for d :upto depth
          :for node = (value-at-depth node-iterator d)
          :for type = (assoc (node-type node)
                             '((quote-node . 1)
                               (quasiquote . 1)
                               (comma . -1)
                               (comma-at . -1)
                               (comma-dot . -1)))
          :when type
            :sum (cdr type))))

(defun quotedp (node-iterator)
  (plusp (quotedness node-iterator)))


;;; Self-evaluating forms

(defun number-node-p (node-iterator)
  (let ((node (value node-iterator)))
    (cond
      ((token-node-p node)
       ;; if it's a token, then we assume it's a decimal number
       (and (null (package-prefix node))
            (null (package-marker node))
            (numberp
             (read-from-string (node-string node-iterator)))))
      ;; TODO binary #b
      ;; TODO octal #o
      ;; TODO hex #x
      ;; TODO complex #c
      )))

(defun self-evaluating-p (node-iterator)
  (let* ((node (value node-iterator))
         (type (node-type node))
         (token-node-p (token-node-p node)))
    #|
 TODO:
- numbers (includes binary, octal, hexa, #r)
- string literals
- character literals
- keywords
- symbols (depends on the environment)
    |#
    (or (and token-node-p
             (or )))))



#|

TODO

- mapcar-form-p
- defpackage-form-p
- uiop/package--define-package-form-p

- current-root-node
- current-top-level-node
- "how many nodes are there before? (a.k.a. the current node is which
  nth child of its parent?)
- how many nodes after? (useful to lint e.g. an `if')
- nth-child-p node-iterator n
  - not "tree-iterator" because I want to skip stuff (not sure if it's a valid reason)
- nth-child-< node-iterator from to : return true if (<= from node's-position (1- to))
- before-current-node

- method "is point at start of form"
- method "is point at end of form"

|#

;; TODO figure out what kind of bindings (:zero-or-more ?x) should produce.
;; 2025-11-25 it's kind-of done, but it needs more tests
#++ (compile-pattern '(if :?cond :?then :?else :?extra (:zero-or-more :?extras)))


(define-node-matcher malformed-if-node-p
    ;; This matches longer sequence because there's no implicit
    ;; "anchor" at the end of the patterns.
    ((if (:maybe (?cond (:maybe (?then (:maybe (?else (:maybe ?extra)))))))))
  (cond
    ;; there should be no extra forms
    (?extra (values t :extra-forms))
    ;; These are mandatory
    ((null ?cond) (values t :missing-cond-and-then))
    ((null ?then) (values t :missing-then))))

;; TODO export
;; TODO test
(defun loop-form-p ($node)
  (with-match ($node ((cl:loop (:named ?body (:maybe _)))))
    ?body))


;;; Enclosing form navigation

;; TODO this generic, it should work for any kind of iterators
(defun normalize-pattern-predicate (matcher &key skipp)
  "Convert MATCHER into a predicate on iterator.

MATCHER may be:
  - a function: returned as-is
  - a symbol: converted into a function that match a list forms whose first element is that symbol
  - a list (pattern): compiled with `breeze.pattern:compile-pattern` and wrapped into a lambda."
  (etypecase matcher
    (function matcher)
    (symbol (normalize-pattern-predicate `(((:symbol ,matcher)))))
    ((or cons vector pattern)
     (let ((compiled-pattern (compile-pattern matcher)))
       (lambda (iterator)
         (match compiled-pattern
           ;; match advances the iterator, but we don't want the
           ;; predicate to modify the input...
           (copy-iterator iterator)
           :skipp skipp))))))

;; TODO maybe add a keyword argument to control whether to return
;; NODE-ITERATOR if it satisfies PREDICATE.
(defun find-enclosing-node (node-iterator matcher
                            &key (direction :bottom-up)
                            &aux (predicate (normalize-pattern-predicate
                                             matcher
                                             :skipp #'whitespace-or-comment-node-p)))
  "Find the first enclosing ancestor of NODE-ITERATOR that satisfies MATCHER.

Returns a an iterator, or NIL if no ancestor matches.

MATCHER is a designator for a function that accepts a
node-iterator (see `normalize-pattern-predicate').

DIRECTION controls the traversal order:
  :bottom-up (default) walk from NODE-ITERATOR towards the root,
  returns the nearest (innermost) ancestor that matches.
  :top-down walk from the root towards NODE-ITERATOR, returns the
  furthest (outermost) ancestor that matches."
  (ecase direction
    (:bottom-up
     (loop :with it := (copy-iterator node-iterator)
           ;; :for before := (copy-iterator it)
           :do (when (funcall predicate it)
                 (return it))
               (unless (go-up it)
                 (return nil))))
    (:top-down
     (loop :with it := (goto-root (copy-iterator node-iterator))
           :do (when (funcall predicate it)
                 (return it))
               (unless (go-down it)
                 (return nil))))))

(defun find-nearest-enclosing-node (node-iterator matcher)
  "Find the closest enclosing node matching MATCHER.
MATCHER may be a symbol, a pattern list, a pattern, or a predicate
function.

Returns a node-iterator or NIL."
  (find-enclosing-node node-iterator matcher :direction :bottom-up))

(defun find-furthest-enclosing-node (node-iterator matcher)
  "Find the most distant enclosing node matching MATCHER.
MATCHER may be a symbol, a pattern list, a pattern, or a predicate
function.

Returns a node-iterator, or NIL."
  (find-enclosing-node node-iterator matcher :direction :top-down))

(defun find-all-enclosing-nodes (node-iterator matcher)
  "Return all enclosing nodes matching MATCHER, nearest-first.
MATCHER may be a symbol, a pattern list, or a predicate function.
Each element is a node-iterator copy positioned at the matching node."
  (loop
    :with predicate := (normalize-pattern-predicate matcher)
    :with it := (copy-iterator node-iterator)
    :while (go-up it)
    :when (funcall predicate it)
      :collect (copy-iterator it)))
