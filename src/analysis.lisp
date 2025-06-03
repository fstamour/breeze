
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

;; (defpattern in-package package-designator)

(defun match-parser-state (pattern state &key skipp)
  (match pattern (make-node-iterator state) :skipp skipp))

(defmethod match (pattern (state state) &key skipp)
  (match-parser-state pattern state :skipp skipp))

(defmethod match ((pattern symbol) (state state) &key skipp)
  (declare (ignore skipp))
  ;; These should return nil because we're trying to match 1 symbol
  ;; against a list of nodes (even if that list is empty).
  nil)

;; TODO move to utils, maybe rename "safe-plusp"
(defun plusp* (x)
  (and (numberp x) (plusp x)))

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
       ;; TODO use case-sensitive comparison, but convert case if
       ;; necessary (i.e. depending on *read-case*)
       ;;
       ;; TODO check if we can find the symbol denoted by
       ;; symbol-node... and compare (eq) symbol with it.
       (when symbol-node
         (and
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
          ;; symbol-node
          t))))))

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
    (breeze.pattern::make-binding pattern (value node-iterator))))

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
;; - case converting if necessary
;; - skip whitespaces
;; - check if there's a package designator
;;
;; Now, I have a chicken-and-egg issue because of
;; package-local-nicknames...  I need to know what is the current
;; package to look for PLNs to find the in-pacakge form, but I need
;; the in-package to know the current package.

;; TODO don't include nodes that are quoted...
(define-node-matcher in-package-node-p ((in-package :?package-designator))
  (get-bindings :?package-designator))


;; TODO WIP
#++
(defun find-nearest-in-package (buffer)
  (let* ((position (point buffer))
         (node-iterator (node-iterator buffer))
         (candidates (top-level-in-package (state node-iterator))))
    (unless candidates
      ;; TODO compute them, put them in a vector so we can
      ;; differentiate between "not computed" and "no in-package
      ;; present"
      )
    (when (and candidates (plusp (length candidates)))
      (find-if (lambda (in-package-node)
                 (< (node-end in-package-node) position))))))


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

(defun node-parse-error (node-iterator  message &key (replacement 'null))
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
  (alexandria:when-let ((package-designator-node (in-package-node-p node-iterator)))
    (and (valid-node-p package-designator-node)
         (let* ((content (node-content (state node-iterator) package-designator-node))
                (package-designator (read-from-string content)))
           (when (and (typep package-designator 'breeze.string:string-designator)
                      (null (find-package package-designator)))
             (node-style-warning
              node-iterator
              (format nil "Package ~s is not currently defined." package-designator)))))))

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
  (loop :until (donep node-iterator)
        :for node = (value node-iterator)
        :for depth = (slot-value node-iterator 'depth)
        :do (when (catch 'cut
                    (error-invalid-node node-iterator)
                    (warn-undefined-in-package node-iterator)
                    (when (and (plusp depth)
                               (whitespace-node-p node))
                      (warn-extraneous-whitespaces node-iterator))
                    t)
              (next node-iterator))))

(defun lint-buffer (buffer &aux (*diagnostics* '()))
  "Apply all the linting rules, and accumulate the \"diagnostics\"."
  (check-type buffer buffer)
  (handler-bind
      ((node-parse-error (lambda (condition)
                           (diag-error (target-node condition)
                                       (simple-condition-format-control condition)
                                       (simple-condition-format-arguments condition))
                           ;; Don't analyze further down that tree... I guess!
                            (next (target-node condition)
                             :dont-recurse-p t)
                           (throw 'cut nil)))
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
