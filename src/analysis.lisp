
(uiop:define-package #:breeze.analysis
    (:documentation "Linter, formatter, and maybe more.")
  (:use #:cl)
  (:use-reexport #:breeze.lossless-reader #:breeze.pattern #:breeze.workspace)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  ;; Tree/Form predicate
  (:export
   #:in-package-node-p
   #:child-of-mapcar-node-p)
  (:export #:map-top-level-forms
           #:map-top-level-in-package)
  (:export
   #:lint-buffer
   #:lint
   #:fix-buffer
   #:after-change-function))

(in-package #:breeze.analysis)


;;; Basic utilities for nodes

(defun node-length (node)
  "Returns the number of children of NODE, or nil if it doesn't have any
children nodes."
  (etypecase node
    (node
     (let ((children (node-children node)))
       (when (nodesp children)
         (length children))))
    (node-iterator (node-length (value node)))))

(defun node-string= (string node &optional state)
  (when node
    (etypecase node
      (node-iterator (node-string-equal string (value node) (state node)))
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

;;; Integrating pattern.lisp and lossless-parser.lisp

(defun match-parser-state (pattern state &key skipp)
  (match pattern (make-node-iterator state) :skipp skipp))

(defmethod match (pattern (state state) &key skipp)
  (match-parser-state pattern state :skipp skipp))

(defmethod match ((pattern symbol) (state state) &key skipp)
  (declare (ignore skipp))
  ;; These should return nil because we're trying to match 1 symbol
  ;; against a list of nodes (even if that list is empty).
  nil)

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
        ;; TODO this doens't check if characters are escpaped,
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
        (push-vector token-node-iterator (node-children node))
        (node-symbol-name token-node-iterator)))
    ((token-node-p node)
      (node-symbol-name node-iterator))))

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

;; TODO add a special pattern type to match symbols in packages that
;; are not defined in the current image.
(defmethod match ((pattern symbol) (node-iterator node-iterator) &key skipp)
  ;; TODO skip nodes
  (match-symbol-to-token pattern node-iterator))

(defmethod match ((pattern null) (node-iterator node-iterator) &key skipp)
  ;; TODO skip nodes
  (match-symbol-to-token pattern node-iterator))

(defmethod match ((pattern term) (state state) &key skipp)
  (match-parser-state pattern state :skipp skipp))

(defmethod match ((pattern term) (node-iterator node-iterator) &key skipp)
  (unless (donep node-iterator)
    (breeze.pattern::make-binding pattern (copy-iterator node-iterator))))

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


;;; Basic tree inspection

;; TODO match-case

(defmacro with-match ((node-iterator pattern) &body body)
  (multiple-value-bind (compiled-pattern term-pool)
      (compile-pattern pattern)
    (let ((bindings (intern (symbol-name '#:bindings) *package*))
          (get-bindings (intern (symbol-name '#:get-bindings) *package*)))
      `(let* ((,bindings (match ,compiled-pattern (copy-iterator ,node-iterator)
                                :skipp #'whitespace-or-comment-node-p)))
         (flet ((,get-bindings (term-name)
                  (when ,bindings
                    (when-let* ((term (gethash term-name ,term-pool))
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
         (flet ((get-bindings (term-name)
                  (when bindings
                    (when-let* ((term (gethash term-name ,term-pool))
                                (binding (find-binding bindings term)))
                      (to binding)))))
           (declare (ignorable (function get-bindings)))
           ,@body)))))

;; TODO I want to check if a node is an "in-package" node...
;; - [ ] case converting if necessary
;; - [x] skip whitespaces
;; - [x] check if there's a package designator
;;
;; Now, I have a chicken-and-egg issue because of
;; package-local-nicknames...  I need to know what is the current
;; package to look for PLNs to find the in-pacakge form, but I need
;; the in-package to know the current package.

(define-node-matcher in-package-node-p ((in-package :?package-designator))
  (unless (quotedp node-iterator)
    (when-let* ((package-designator (get-bindings :?package-designator))
                (package-designator-node (value package-designator)))
      ;; TODO string-designator-node-p
      (when (or (token-node-p package-designator-node)
                (string-node-p package-designator-node)
                (sharp-uninterned-node-p package-designator-node))
        ;; TODO else... it's a malformed in-package form
        package-designator))))

#|

TODO

- loop-form-p
- mapcar-form-p
- defpackage-form-p
- uiop/package--define-package-form-p

|#


(defun child-of-mapcar-node-p (node-iterator)
  ;; TODO this is not exactly right (it won't detect "cl:mapcar"), but
  ;; this will do for now™
  (node-string-equal "mapcar"
                     (first-node (parent-node node-iterator))
                     (state node-iterator)))


#++ (compile-pattern '(if :?cond :?then :?else :?extra (:zero-or-more :?extras)))

(define-node-matcher malformed-if-node-p
    ;; TODO (or (if) (if ?cond) ...)
    ((if :?cond :?then :?else :?extra #++ (:zero-or-more :?extras)))
  (when bindings
    ;; (destructuring-bind (&key ?cond ?then ?else ?extra) bindings)
    t))


;;; TODO Quotep

;; - look at the "path", from the top-level
;; - if there's any =(quote ...= node, then it is quoted
;; - if there are "quasiquotes", it's more complicated...

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


;; TODO add tests
(defun map-top-level-forms (function buffer)
  ;; TODO Recurse inot forms that "preserves" top-level-ness:
  ;; progn, locally, macrolet, symbol-macrolet, eval-when
  (loop :with iterator = (make-node-iterator buffer)
        :until (donep iterator)
        :do (let ((node (value iterator)))
              (unless (whitespace-or-comment-node-p node)
                (funcall function iterator)))
            (incf (pos iterator))))

;; TODO add tests
(defun map-top-level-in-package (function buffer)
  "Map FUNCTION over all top-level (in-package ...) forms in BUFFER."
  (map-top-level-forms
   (lambda (node-iterator)
     (when-let ((package-name (in-package-node-p node-iterator)))
       (funcall function node-iterator package-name)))
   buffer))


;;; In-package and package defintions



#++
(defun check-in-package ()
  "Make sure the previous in-package form desginates a package that can
be found. If it's not the case (e.g. because the user forgot to define
a package and/or evaluate the form that defines the package) they show
a message and stop the current command."
  (let ((package (current-package)))
    (if package
        (let ((package-name (breeze.analysis::node-string-designator-string
                             package)))
          (unless (find-package package-name)
            (message "The nearest in-package form designates a package that doesn't exists: ~s"
                     package-name)
            (return-from-command)))
        (;; TODO message no (in-package ...) found...
         (return-from-command)))))

;; TODO add tests
(defmethod index-in-package-nodes ((buffer buffer))
  (setf (in-package-nodes buffer)
        (coerce (uiop:while-collecting (form)
                  (map-top-level-in-package
                   (lambda (node-iterator package-name-designator)
                     (declare (ignore node-iterator))
                     (form package-name-designator))
                   buffer))
                'vector)))

;; TODO add tests
(defmethod current-package ((buffer buffer))
  (let ((position (point buffer))
        (candidates (or
                     (in-package-nodes buffer)
                     (index-in-package-nodes buffer))))
    (when (and candidates (plusp (length candidates)))
      (find-if (lambda (node) (< (end node) position)) candidates))))

;; TODO add tests
(defmethod locate-package-definition ((package package))
  (locate-package-definition (package-name package)))

;; TODO add tests
(defmethod locate-package-definition ((package node-iterator))
  (locate-package-definition (node-string-designator-string package)))

;; TODO add tests
(defmethod locate-package-definition ((package string))
  ;; TODO don't look only in the current buffer
  ;; TODO maybe cache the "match data" on the defpackage macros
  (when-let ((buffer (breeze.command:current-buffer)))
    (map-top-level-forms
     (lambda (node-iterator)
       (with-match (node-iterator
                    (:alternation
                     (cl:defpackage ?name)
                     (uiop:define-package ?name)))
         (when-let* ((package-name-node (get-bindings '?name))
                     (package-name (node-string-designator-string
                                    package-name-node)))
           (when (string= package package-name)
             (return-from locate-package-definition node-iterator)))))
     buffer)))



;;; Utilities to collect "diagnostics"

(defvar *diagnostics* nil)
(defvar *point-max* nil)

(defun make-diagnostic (start end severity format-string format-args)
  "Create a \"diagnostic\" object."
  (list start (if (= +end+ end) *point-max* end)
        severity
        (apply #'format nil format-string format-args)))

(defun push-diagnostic* (start end severity format-string format-args)
  "Create a diagnostic object and push it into the special variable
*diagnostics*."
  (let ((diagnostic (make-diagnostic start end
                                     severity
                                     format-string format-args)))
    (push diagnostic *diagnostics*)
    diagnostic))

;; Same as push-diagnostic*, but takes a &rest
(defun push-diagnostic (start end severity format-string &rest format-args)
  "Create a diagnostic object and push it into the special variable
*diagnostics*."
  (push-diagnostic* start end severity format-string format-args))

(defun diag-node (node severity format-string &rest format-args)
  "Create a diagnostic object for NODE and push it into the special
variable *diagnostics*."
  (push-diagnostic* (start node) (end node)
                    severity format-string format-args))

(defun diag-warn (node format-string &rest format-args)
  "Create a diagnostic object for NODE with severity :WARNING and push it
into the special variable *diagnostics*."
  (apply #'diag-node node :warning format-string format-args))

(defun diag-error (node format-string &rest format-args)
  "Create a diagnostic object for NODE with severity :error and push it
into the special variable *diagnostics*."
  (apply #'diag-node node :error format-string format-args))


;;; using signals to be able to decide what to do when certain
;;; conditions are discovered (e.g. keep note of it v.s. fixing it)

#|

Which conditions should I use?

simple-error
simple-warning

parse-error
style-warning

simple-condition-format-control, simple-condition-format-arguments

(apply #'format nil (simple-condition-format-control foo)
(simple-condition-format-arguments foo))

|#

(define-condition simple-node-condition (simple-condition)
  ((node-iterator :initarg :node-iterator :reader target-node)
   (replacement :initarg :replacement :reader replacement :initform 'cl:null)))

(defun replacementp (simple-node-condition)
  (not (eq (replacement simple-node-condition) 'cl:null)))

(define-condition simple-node-error (simple-node-condition simple-error) ())

(define-condition simple-node-warning (simple-node-condition simple-warning) ())

(define-condition node-parse-error (simple-node-error parse-error) ())

(defun node-parse-error (node-iterator message &key (replacement 'null))
  (signal (make-condition
           'node-parse-error
           :node-iterator (copy-iterator node-iterator)
           :format-control message
           :replacement replacement)))

(define-condition node-style-warning (simple-node-warning style-warning) ())

(defun node-style-warning (node-iterator message &key (replacement 'null))
  (signal (make-condition
           'node-style-warning
           :node-iterator (copy-iterator node-iterator)
           :format-control message
           :replacement replacement)))

#++
(let ((c (node-style-warning (node 'dummy 0 1) "Bad node ~a")))
  (apply #'format nil (simple-condition-format-control c)
         (target-node c)
         (simple-condition-format-arguments c)))


;;; Linter rules

;; TODO this rule only make sense when "in-image"
(defun warn-undefined-in-package (node-iterator)
  (alexandria:when-let* ((package-designator-node (in-package-node-p node-iterator))
                         (package-name (node-string-designator-string
                                        package-designator-node)))
    (unless (find-package package-name)
      (node-style-warning
       node-iterator
       (format nil "Package ~s is not currently defined." package-name)))))

(defun warn-extraneous-whitespaces (node-iterator)
  (let ((firstp (firstp node-iterator))
        (lastp (lastp node-iterator)))
    (cond
      ((and firstp lastp)
       (node-style-warning
        node-iterator "Extraneous whitespaces."
        :replacement nil))
      (firstp
       (node-style-warning
        node-iterator "Extraneous leading whitespaces."
        :replacement nil))
      ((and lastp (not (line-comment-node-p
                        (previous-sibling node-iterator))))
       (node-style-warning
        node-iterator "Extraneous trailing whitespaces."
        :replacement nil))
      ((and (not (or firstp lastp))
            ;; Longer than 1
            (< 1 (- (end node-iterator) (start node-iterator)))
            ;; "contains no newline"
            (not (position #\Newline
                           (source node-iterator)
                           :start (start node-iterator)
                           :end (end node-iterator)))
            ;; is not followed by a line comment
            (not (line-comment-node-p
                  (next-sibling node-iterator))))
       (node-style-warning
        node-iterator "Extraneous internal whitespaces."
        :replacement " ")))))

(defun error-invalid-node (node-iterator)
  (unless (valid-node-p (value node-iterator))
    (node-parse-error node-iterator "Syntax error")))

(defun analyse (buffer
                &aux (node-iterator (make-node-iterator buffer)))
  "Apply all the linting rules."
  (check-type buffer buffer)
  ;; TODO warn when using double colon for external symbols (e.g. cl::defun)
  (loop :until (donep node-iterator)
        :for node = (value node-iterator)
        :for depth = (slot-value node-iterator 'depth)
        :do (progn
              (error-invalid-node node-iterator)
              (warn-undefined-in-package node-iterator)
              (when (and (plusp depth)
                         (whitespace-node-p node))
                (warn-extraneous-whitespaces node-iterator)))
            (next node-iterator)))

(defun lint-buffer (buffer &aux (*diagnostics* '()))
  "Apply all the linting rules, and accumulate the \"diagnostics\"."
  (check-type buffer buffer)
  (handler-bind
      ((node-parse-error (lambda (condition)
                           (diag-error (target-node condition)
                                       (simple-condition-format-control condition)
                                       (simple-condition-format-arguments condition))))
       (node-style-warning (lambda (condition)
                             (diag-warn (target-node condition)
                                        (simple-condition-format-control condition)
                                        (simple-condition-format-arguments condition)))))
    (analyse buffer))
  *diagnostics*)

(breeze.command:define-command lint ()
  "Lint the current buffer."
  (breeze.command:return-value-from-command
   (lint-buffer (breeze.command:current-buffer))))

(defun fix-buffer (buffer)
  (check-type buffer buffer)
  (uiop:while-collecting (conditions)
    (handler-bind ((simple-node-condition
                     (lambda (condition)
                       (when (replacementp condition)
                         (conditions condition)))))
      (analyse buffer))))


;;; Incremental parsing (the interface with the editor at least)

(defun push-edit (edit)
  (declare (ignore edit))
  #++ (print edit))

;; TODO keep track of the buffers/files, process these kind of edits
;; "object":
;;
;; (:DELETE-AT 18361 1)
;; (:INSERT-AT 17591 ";")

(defun breeze.analysis:after-change-function (start stop length &rest rest
                                              &key
                                                buffer-name
                                                buffer-file-name
                                                insertion
                                              &allow-other-keys)
  (declare (ignorable start stop length rest buffer-file-name insertion)) ; yea, you heard me
  ;; TODO the following form is just a hack to keep the breeze's
  ;; buffers in sync with the editor's buffers
  (when-let ((buffer (find-buffer buffer-name)))
    (setf (node-iterator buffer) nil))
  ;; consider ignore-error + logs, because if something goes wrong in
  ;; this function, editing is going to be funked.
  (push-edit
   (cond
     ((zerop length)
      (list :insert-at start insertion))
     ((plusp length)
      (list :delete-at start length))
     (t :unknown-edit))))

;; TODO add NOTE: "can't splice comment", but I wish I could
;; e.g.  `  ;; (some | code)`
;; paredit-splice-sexp or paredit-splice-sexp-killing-backward
