
(uiop:define-package #:breeze.analysis
    (:documentation "Pattern matching against parse trees")
  (:use #:cl)
  (:use-reexport #:breeze.generics
                 #:breeze.lossless-reader
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
        ((string-node-p node) (node-string node-iterator))
        ((sharp-uninterned-node-p node)
         (let (($node (copy-iterator node-iterator)))
           (go-down $node)
           (second (parse-symbol-node $node))))
        ((token-node-p node)
         (second (parse-symbol-node node-iterator)))))))


;;; Integrating pattern.lisp and lossless-parser.lisp

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

;; TODO would be nice to cache this
(defun parse-symbol-node ($node)
  "Extract information about the package-name and symbol-name of a token, if it can.
Returns a list (TYPE SYMBOL-NAME) or (TYPE SYMBOL-NAME PACKAGE-NAME).
PACKAGE-NAME is provided for the types :qualified-symbol and :possibly-internal-symbol.
TYPE is one of:

 - :current-package-symbol
 - :keyword
 - :uninterned-symbol
 - :qualified-symbol
 - :possibly-internal-symbol"
  (unless (donep $node)
    (when-let ((parsed-symbol (parse-symbol (source $node) (start $node) (end $node))))
      (destructuring-bind (type symbol-name &optional package-name)
          parsed-symbol
        ;; TODO apply readtable-case rules on symbol-name
        ;; https://www.lispworks.com/documentation/HyperSpec/Body/23_ab.htm
        (let ((*package* (find-package '#:KEYWORD)))
          ;; HACK: use `cl:read-from-string' to "apply" the
          ;; case-conversion and take care of escaping.
          (ignore-errors
           `(,type
             ,(symbol-name (read-from-string symbol-name))
             ,@(when package-name
                 (list (symbol-name (read-from-string package-name)))))))
        ;; TODO this doesn't check if characters are escpaped,
        ;; TODO this doesn't remove the escape characters...
        #++(ecase (readtable-case *readtable*)
             (:upcase #| this is the default |#
              (string-upcase symbol-name))
             (:downcase (string-downcase symbol-name))
             (:preserve symbol-name)
             (:invert (string-in)))))))

(defun match-symbol-to-token (symbol $node)
  "Match a symbol against a parse tree's node."
  (check-type $node node-iterator)
  (check-type symbol symbol)
  (match (sym (symbol-package symbol)
              (symbol-name symbol))
    (node-string $node)))

(defmethod match ((pattern sym) (node-iterator node-iterator) &key skipp)
  (breeze.pattern::skip node-iterator skipp)
  ;; TODO ensure *current-package* ??
  (match pattern (node-string node-iterator)))

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
         (get-bindings (intern (symbol-name '#:get-bindings))))
    `(let* ((,bindings (match ,compiled-pattern (copy-iterator ,node-iterator)
                         :skipp #'whitespace-or-comment-node-p)))
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

- loop-form-p
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

|#


(defun child-of-mapcar-node-p (node-iterator)
  (declare (ignorable node-iterator))
  ;; TODO with-match 'mapcar
  )


#++ (compile-pattern '(if :?cond :?then :?else :?extra (:zero-or-more :?extras)))

(define-node-matcher malformed-if-node-p
    ;; TODO (or (if) (if ?cond) ...)
    ((if :?cond :?then :?else :?extra #++ (:zero-or-more :?extras)))
  (when bindings
    ;; (destructuring-bind (&key ?cond ?then ?else ?extra) bindings)
    t))
