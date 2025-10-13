
(uiop:define-package #:breeze.analysis
    (:documentation "Pattern matching against parse trees")
  (:use #:cl)
  (:use-reexport #:breeze.generics
                 #:breeze.parser
                 #:breeze.pattern)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:export #:node-string-designator
           #:parse-symbol-node)
  ;; Tree/Form predicate
  (:export #:with-match
           #:define-node-matcher
           #:child-of-mapcar-node-p
           #:quotedp))

(in-package #:breeze.analysis)


;;; Basic utilities for nodes

(defun node-string= (string node &optional state)
  (when node
    (etypecase node
      (node-iterator (node-string= string (value node) (state node)))
      (node
       (string= (source state) string
                :start1 (start node)
                :end1 (end node))))))

(defun node-string-equal (string $node)
  (when $node
    (string-equal (source $node) string
                  :start1 (start $node)
                  :end1 (end $node))))

(defun node-string-designator (node-iterator)
  "Return the string designated by node-iterator."
  (check-type node-iterator node-iterator)
  (unless (donep node-iterator)
    (let ((node (value node-iterator)))
      (cond
        ;; TODO handle "`a" "' #| 🤯 |# a" (quote-node and quasiquote
        ;; TODO (hard) handle "#."
        ((string-node-p node) (node-string node-iterator))
        ((sharp-uninterned-node-p node)
         (let (($node (copy-iterator node-iterator)))
           (go-down $node)
           (second (parse-symbol-node $node))))
        ((token-node-p node)
         (second (parse-symbol-node node-iterator)))))))


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

(defun parse-symbol-node ($node)
  "Extract information about the package-name and symbol-name of a token, if it can.
Returns a list (TYPE SYMBOL-NAME) or (TYPE SYMBOL-NAME PACKAGE-NAME).
PACKAGE-NAME is provided for the types :qualified and :possibly-internal.
TYPE is one of:

 - :current
 - :keyword
 - :uninterned
 - :qualified
 - :possibly-internal"
  (unless (or (donep $node)
              (not (valid-node-p (value $node))))
    (cond
      ;; TODO support () === nil
      ;; ((parens-node-p (value $node)))
      ((sharp-uninterned-node-p (value $node))
       `(:uninterned ,(name (children $node))))
      ((token-node-p (value $node))
       (with-slots (package-prefix package-marker name)
           (value $node)
         (let ((marker-length (length package-marker)))
           (cond
             ((and (null package-prefix)
                   (null package-marker))
              `(:current ,name))
             ((and package-marker
                   (= 1 marker-length)
                   (or (null package-prefix)
                       (string= "KEYWORD" package-prefix)))
              `(:keyword ,name))
             ((and package-marker
                   (= 1 marker-length))
              `(:qualified ,name ,package-prefix))
             ((and package-marker
                   (= 2 marker-length))
              `(:possibly-internal ,name ,package-prefix)))))))))

(defun same-package-p (package-designator1 package-designator2)
  "Check if both package-designators designates the same package.
The designators can be strings, symbols or packages."
  (or
   (eq package-designator1 package-designator2)
   (and
    (not (packagep package-designator1))
    (not (packagep package-designator2))
    ;; TODO choose between string= and string-equal depending on
    ;; *read-case*
    (string-equal package-designator1 package-designator2))
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
      ;; TODO choose between string= and string-equal
      ;; depending on *read-case*
      (string-equal symbol-name-pattern symbol-name)))

(defun match-qualification (qualification-pattern qualification)
  (or (wildp qualification-pattern)
      (eq qualification-pattern qualification)))

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

(defmethod match ((pattern term) (state state) &key skipp)
  "Match a term againt a parse state."
  (match-parser-state pattern state :skipp skipp))

;; TODO One method per type of node
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

(defmacro with-match ((node-iterator pattern) &body body)
  (let* ((compiled-pattern (compile-pattern pattern))
         (bindings (intern (symbol-name '#:bindings)))
         (get-bindings (intern (symbol-name '#:get-bindings)))
         ($node (gensym "$node")))
    `(let* ((,$node ,node-iterator)
            (,bindings
              (and ,$node
                   (not (donep ,$node))
                   (match ,compiled-pattern (copy-iterator ,$node)
                     :skipp #'whitespace-or-comment-node-p))))
       (flet ((,get-bindings (name)
                (when ,bindings
                  (when-let ((binding (find-binding ,bindings name)))
                    (to binding)))))
         (declare (ignorable (function ,get-bindings)))
         ,@body))))

;; TODO be able to name the "node-iterator" argument
;; TODO maybe, be able to do some checks _before_ trying to match
;; TODO maybe, add some options for common stuff ... e.g. "don't match if quoted"
(defmacro define-node-matcher (name (pattern) &body body)
  (let ((compiled-pattern (compile-pattern pattern)))
    `(defun ,name (node-iterator)
       ,(format nil "Does NODE-ITERATOR match ~s?" pattern)
       (let* ((bindings (match ,compiled-pattern (copy-iterator node-iterator)
                          :skipp #'whitespace-or-comment-node-p)))
         (flet ((get-bindings (name)
                  (when bindings
                    (when-let ((binding (find-binding bindings name)))
                      (to binding)))))
           (declare (ignorable (function get-bindings)))
           ,@body)))))


;;; Quoting

;; TODO this takes care of the ` reader-macro, but not the (quote ...) special operator :/
;; TODO nor can if figure out if part of a macro is evaluated or not
(defun quotedness (node-iterator)
  (let ((depth (slot-value node-iterator 'depth)))
    (loop :for d :upto depth
          :for node = (value-at-depth node-iterator d)
          :for type = (assoc (node-type node)
                             '((quote . 1)
                               (quasiquote . 1)
                               (comma . -1)
                               (comma-at . -1)
                               (comma-dot . -1)))
          :when type
            :sum (cdr type))))

(defun quotedp (node-iterator)
  (plusp (quotedness node-iterator)))



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


(defun child-of-mapcar-node-p ($node)
  (with-match ($node ((cl:mapcar)))
    ;; TODO also check _where_ is node in the "mapcar" form
    bindings))

;; TODO figure out what kind of bindings (:zero-or-more ?x) should produce.
#++ (compile-pattern '(if :?cond :?then :?else :?extra (:zero-or-more :?extras)))

(define-node-matcher malformed-if-node-p
    ;; TODO (or (if) (if ?cond) ...)
    ((if :?cond :?then :?else :?extra #++ (:zero-or-more :?extras)))
  (when bindings
    ;; (destructuring-bind (&key ?cond ?then ?else ?extra) bindings)
    t))

;; TODO export
;; TODO test
(defun loop-form-p ($node)
  (with-match ($node ((cl:loop)))
    bindings))
