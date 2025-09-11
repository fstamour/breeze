
(uiop:define-package #:breeze.analysis
    (:documentation "Pattern matching against parse trees")
  (:use #:cl)
  (:use-reexport #:breeze.lossless-reader #:breeze.pattern)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:export #:node-symbol-name
           #:node-string-designator-string)
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

(defun node-string-equal (string node &optional state)
  (when node
    (etypecase node
      (node-iterator (node-string-equal string (value node) (state node)))
      (node
       (string-equal (source state) string
                     :start1 (start node)
                     :end1 (end node))))))

;; TODO add tests
(defun node-symbol-name (token-node-iterator
                         &aux (token-node (value token-node-iterator)))
  ;; TODO would be nice to cache this
  (when (token-node-p token-node)
    (let ((state (state token-node-iterator)))
      (when-let* (;; TODO would be nice to cache this
                  (symbol-node (token-symbol-node state token-node))
                  (symbol-name
                   (ecase (node-type symbol-node)
                     (current-package-symbol (node-content state token-node))
                     ((keyword uninterned-symbol) (node-content state symbol-node))
                     ((qualified-symbol possibly-internal-symbol)
                      (let* ((nodes (node-children symbol-node))
                             ;; (package-name-node (first-node nodes))
                             (symbol-name-node (second-node nodes)))
                        (node-content state symbol-name-node))))))
        ;; TODO apply readtable-case rules on symbol-name
        ;; https://www.lispworks.com/documentation/HyperSpec/Body/23_ab.htm
        (let ((*package* (find-package '#:KEYWORD)))
          (ignore-errors
           (symbol-name (read-from-string symbol-name))))
        ;; TODO this doesn't check if characters are escpaped,
        ;; TODO this doesn't remove the escape characters...
        #++(ecase (readtable-case *readtable*)
             (:upcase #| this is the default |#
              (string-upcase symbol-name))
             (:downcase (string-downcase symbol-name))
             (:preserve symbol-name)
             (:invert (string-in)))))))

;; TODO add tests
(defun node-string-designator-string (node-iterator
                                      &aux (node (value node-iterator)))
  "Return the name of the symbol designated by node-iterator."
  (check-type node-iterator node-iterator)
  (cond
    ((string-node-p node) (node-string node-iterator))
    ((sharp-uninterned-node-p node)
      (let ((token-node-iterator (copy-iterator node-iterator)))
        ;; TODO there's probably a function for that...
        (push-subtree token-node-iterator (node-children node))
        (node-symbol-name token-node-iterator)))
    ((token-node-p node)
      (node-symbol-name node-iterator))))


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


(defun match-symbol-to-token (symbol node-iterator)
  (check-type node-iterator node-iterator)
  (let ((token-node (value node-iterator))
        (state (state node-iterator)))
    (and
     (symbolp symbol)
     (token-node-p token-node)
     (let* ((name (symbol-name symbol))
            (package (symbol-package symbol))
            ;; TODO would be nice to cache this
            (symbol-node (token-symbol-node state token-node)))
       ;; TODO use node-symbol-name
       (and
        symbol-node
        (ecase (node-type symbol-node)
          (current-package-symbol (node-string-equal name token-node state))
          (keyword
           (and (string-equal "KEYWORD" (package-name package))
                (node-string-equal name symbol-node state)))
          (uninterned-symbol
           (and (null package)
                (node-string-equal name symbol-node state)))
          ((qualified-symbol possibly-internal-symbol)
           (let* ((nodes (node-children symbol-node))
                  (package-name-node (first-node nodes))
                  (symbol-name-node (second-node nodes)))
             (and
              (node-string-equal name symbol-name-node state)
              (some (lambda (package-name)
                      (node-string-equal package-name package-name-node state))
                    `(,(package-name package)
                      ,@(package-nicknames package)))))))
        t)))))

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
symbol nodes without having to define packages or intern symbols."))

(defmethod print-object ((sym sym) stream)
  (format stream "(sym ~s ~s~@[ ~s~])"
          (sym-package sym)
          (name sym)
          (qualification sym)))

(defun sym (package name &optional qualification)
  ;; TODO validation
  ;; nil means "don't care"
  (check-type package (or string (member :any nil :keyword)))
  (check-type name (or string (member :any)))
  (check-type qualification (member nil :current :internal :uninterned))
  (make-instance 'sym
                 :package package
                 :name name
                 :qualification qualification))

(defmethod match ((pattern symbol) (node-iterator node-iterator) &key skipp)
  (when skipp (breeze.pattern::skip node-iterator skipp))
  (match-symbol-to-token pattern node-iterator))

(defmethod match ((pattern null) (node-iterator node-iterator) &key skipp)
  (when skipp (breeze.pattern::skip node-iterator skipp))
  (match-symbol-to-token pattern node-iterator))

(defmethod match ((pattern term) (state state) &key skipp)
  (match-parser-state pattern state :skipp skipp))

(defmethod match ((pattern term) (node-iterator node-iterator) &key skipp)
  (when skipp (breeze.pattern::skip node-iterator skipp))
  (unless (donep node-iterator)
    (breeze.pattern::make-binding pattern (copy-iterator node-iterator))))

(defmethod match ((pattern repetition) (iterator node-iterator) &key skipp)
  (when skipp (breeze.pattern::skip iterator skipp))
  (unless (donep iterator)
    (loop
      ;; :with bindings := t ;; (make-binding-set)
      :with $pattern := (make-pattern-iterator
                         (repetition-pattern pattern))
      ;; TODO update node-iterator on match
      :with $input := (copy-iterator iterator)
      :for $prev-input := (copy-iterator $input)
        :then (copy-iterator $input $prev-input)
      :for i :from 0 :below 100 ;; TODO removve infinite loop guard

      :for new-bindings = (progn
                            (reset $pattern)
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
           ;; (break "The end")
           (return
             (when (<= (repetition-min pattern) i)
               ;; (break "i: ~s new-bindings: ~s" i new-bindings)
               (if (and (zerop i) (not new-bindings))
                   t
                   (let (($start (copy-iterator iterator))
                         ($end
                           ;; if it was a match, include the current position,
                           ;; otherwise stop at the previous one.
                           (if new-bindings $input $prev-input)))
                     ;; update iterator
                     (copy-iterator $end iterator)
                     ;; TODO return an object (iterator-range? +
                     ;; binding-sets???)
                     (make-binding
                      pattern
                      (list
                       :bindings bindings
                       :$start $start
                       :$end $end
                       :times i)))))))
      #++
       (when new-bindings
         ;; collect all the bindings
         ;; (setf bindings (merge-bindings bindings new-bindings))
         ;; TODO check if bindings is nil after merging.
         (unless bindings
           (break "conflict?"))))))

;; TODO package-local-nicknames

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
  (multiple-value-bind (compiled-pattern term-pool)
      (compile-pattern pattern)
    (let ((bindings (intern (symbol-name '#:bindings) *package*))
          (get-bindings (intern (symbol-name '#:get-bindings) *package*)))
      `(let* ((,bindings (match ,compiled-pattern (copy-iterator ,node-iterator)
                                :skipp #'whitespace-or-comment-node-p)))
         (flet ((,get-bindings (name)
                  (when ,bindings
                    (when-let* ((term (gethash name ,term-pool))
                                (binding (find-binding ,bindings term)))
                      (to binding)))))
           (declare (ignorable (function ,get-bindings)))
           ,@body)))))

;; TODO be able to name the "node-iterator" argument
;; TODO maybe, be able to do some checks _before_ trying to match
;; TODO maybe, add some options for common stuff ... e.g. "don't match if quoted"
(defmacro define-node-matcher (name (pattern) &body body)
  (multiple-value-bind (compiled-pattern term-pool)
      (compile-pattern pattern)
    `(defun ,name (node-iterator)
       ,(format nil "Does NODE-ITERATOR match ~s?" pattern)
       (let* ((bindings (match ,compiled-pattern (copy-iterator node-iterator)
                          :skipp #'whitespace-or-comment-node-p)))
         (flet ((get-bindings (name)
                  (when bindings
                    (when-let* ((term (gethash name ,term-pool))
                                (binding (find-binding bindings term)))
                      (to binding)))))
           (declare (ignorable (function get-bindings)))
           ,@body)))))


;;; Quoting

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
