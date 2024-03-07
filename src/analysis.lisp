
(uiop:define-package #:breeze.analysis
    (:documentation "Linter, formatter, and maybe more.")
  (:use #:cl)
  (:use-reexport #:breeze.lossless-reader #:breeze.pattern)
  ;; Tree/Form predicate
  (:export
   #:in-package-node-p
   #:find-node
   #:find-path-to-position)
  (:export
   #:in-package-node-p
   #:lint))

(in-package #:breeze.analysis)


;;; Integrating pattern.lisp and lossless-parser.lisp

(defparameter *state* nil
  "The parser state associated with the node currently being matched.")

;; (defpattern in-package package-designator)

(defun match-node (pattern state node)
  (let ((*state* state))
    (match pattern node)))

(defun match-parser-state (pattern state)
  (let* ((*state* state))
    (match pattern (tree state))))

(defmethod match (pattern (state state))
  (match-parser-state pattern state))

(defun plusp* (x)
  (and (numberp x) (plusp x)))

(defun match-symbol-to-token (symbol token-node)
  (and
   (symbolp symbol)
   (token-node-p token-node)
   (let* ((name (symbol-name symbol))
          (package (symbol-package symbol))
          (string (node-content *state* token-node)))
     ;; TODO use case-sensitive comparison, but convert case if
     ;; necessary (i.e. depending on *read-case*)
     (if (plusp* (position #\: string))
         (destructuring-bind (package-name symbol-name)
             (remove-if #'alexandria:emptyp
                        (uiop:split-string string :separator '(#\:)))
           (and (member package-name
                        `(,(package-name package)
                          ,@(package-nicknames package))
                        :test #'string-equal)
                (string-equal name symbol-name)))
         (string-equal name string)))))

;; TODO add a special pattern type to match symbols in packages that
;; are not defined in the current image.
(defmethod match ((pattern symbol) (node node))
  (match-symbol-to-token pattern node))

(defmethod match ((pattern null) (node node))
  (match-symbol-to-token pattern node))

(defmethod match ((pattern term) (state state))
  (match-parser-state pattern state))

;; TODO package-local-nicknames


;; TODO One method per type of node

(defmethod match (pattern (node node))
  (case (node-type node)
    ;; Recurse into nodes of type "parens"
    (parens (match pattern (node-children node)))
    (t (call-next-method))))

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

(defun node-length (node)
  "Returns the number of children of NODE, or nil if it doesn't have any
children nodes."
  (let ((children (node-children node)))
    (when (listp children)
      (length children))))

;; TODO I want to check if a node is an "in-package" node...
;; - case converting if necessary
;; - skip whitespaces
;; - check if there's a package designator
;;
;; Now, I have a chicken-and-egg issue because of
;; package-local-nicknames...  I need to know what is the current
;; package to look for PLNs to find the in-pacakge form, but I need
;; the in-package to know the current package.

(defun in-package-node-p (state node)
  "Is NODE a cl:in-package node?"
  (let* ((*state* state)
         (*match-skip* #'whitespace-or-comment-node-p)
         (bindings (match #.(compile-pattern `(in-package :?package)) node)))
    (when bindings
      (destructuring-bind (term package-designator-node) bindings
        (declare (ignore term))
        package-designator-node))))

(defun find-node (position nodes)
  "Given a list of NODES, return which node contains the POSITION."
  (when (listp nodes)
    (loop :for node :in nodes
          :for start = (node-start node)
          :for end = (node-end node)
          :for i :from 0
          :when (and (<= start end) (< position end))
            :do (return (cons node i)))))

(defun find-path-to-position (state position)
  "Given a list of NODES, return a path (list of cons (node . index))"
  (loop :for found = (find-node position (tree state))
          :then (find-node position (node-children (car found)))
        #++ (let ((node (car found)))
              (and (listp (node-content state node))
                   ;; (car (node-content state node))
                   (find-node position (node-content state node))))
        :while found
        :collect found))


;;; Trying to figure out how to run the "formatting rules" without
;;; applying them...

(defun %walk (state callback tree depth)
  (when tree
    (flet ((cb (node &rest args)
             "Call callback with NODE, DEPTH and ARGS."
             (apply callback node :depth depth args)))
      (etypecase tree
        (list
         (loop
           :for i :from 0
           :for previous = nil :then (first rest)
           :for rest :on tree
           :for node = (car rest)
           ;; Recurse
           :collect (%walk state
                           callback
                           (cb node
                               :aroundp t
                               :nth i
                               :firstp (eq tree rest)
                               :lastp (null (cdr rest))
                               :previous previous)
                           (1+ depth))))
        (node
         (case (node-type tree)
           (parens
            (cb tree :beforep t)
            (%walk state
                   callback
                   (node-children tree)
                   (1+ depth))
            (cb tree :afterp t))
           (t
            (cb tree))))))))

(defun walk (state callback)
  "Call CALLBACK over all nodes in the parse tree contained by STATE.
CALLBACK will be called multiple times on the same node, with
different parameters.

When CALLBACK is called with :aroundp t, the CALLBACK can decide to
stop the walk here(i.e. not recurse) by returning nil. The CALLBACK
can also return a new node altogether, the walk will
continue (recurse) in this new node instead.

"
  (%walk state callback (tree state) 0))

;; This is equivalent to unparse with the leading and trailing
;; whitespace fixes. It is _much_ more succint!
#++
(let ((state (parse " (+ 2) ")))
  (with-output-to-string (out)
    (walk state (lambda (node &rest args &key depth aroundp beforep afterp
                                           firstp lastp nth)
                  ;; Debug info
                  (format t "~&~s ~{~s~^ ~}" node args)
                  ;; Printing stuff
                  (cond
                    (beforep
                     (write-char #\( out))
                    (afterp
                     (write-char #\) out))
                    ((not (or aroundp beforep afterp))
                     (write-node node state out)))
                  ;; Removing useless whitespaces
                  (unless (and (plusp depth)
                               aroundp
                               (whitespace-node-p node)
                               (or firstp lastp))
                    node)))))


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
  (push
   (make-diagnostic start end
                    severity
                    format-string format-args)
   *diagnostics*))

;; Same as push-diagnostic*, but takes a &rest
(defun push-diagnostic (start end severity format-string &rest format-args)
  "Create a diagnostic object and push it into the special variable
*diagnostics*."
  (push-diagnostic* start end  severity format-string format-args))

(defun diag-node (node severity format-string &rest format-args)
  (push-diagnostic* (node-start node) (node-end node)
                    severity format-string format-args))

(defun diag-warn (node format-string &rest format-args)
  (apply #'diag-node node :warning format-string format-args))

(defun diag-error (node format-string &rest format-args)
  (apply #'diag-node node :error format-string format-args))



(defun warn-undefined-in-package (state node)
  (alexandria:when-let ((package-designator-node (in-package-node-p state node)))

    (let* ((package-designator (read-from-string (node-content state package-designator-node)))
           (package (find-package package-designator)))
      (unless package
        (diag-warn
         node
         "Package ~s is not currently defined." package-designator)))))

(defun warn-extraneous-whitespaces (node firstp lastp previous)
  (cond
    ((and firstp lastp)
     (diag-warn node "Extraneous whitespaces."))
    (firstp
     (diag-warn node "Extraneous leading whitespaces."))
    ((and lastp (not (line-comment-node-p previous)))
     (diag-warn node "Extraneous trailing whitespaces."))))

(defun error-invalid-node (node)
  (unless (valid-node-p node)
    (diag-error node "Syntax error")))

(defun lint (&key buffer-string point-max &allow-other-keys
             &aux
               (state (parse buffer-string))
               (*diagnostics* '())
               (*point-max* (or point-max (length buffer-string))))
  (walk state
        (lambda (node &rest args &key depth aroundp beforep afterp
                                   firstp lastp nth
                                   previous)
          (declare (ignorable depth beforep afterp nth args))
          ;; Debug info
          ;; (format *debug-io* "~&~s ~{~s~^ ~}" node args)
          (when aroundp
            (error-invalid-node node)
            (warn-undefined-in-package state node)
            (when (and (plusp depth)
                       (whitespace-node-p node))
              (warn-extraneous-whitespaces node firstp lastp previous)))
          ;; Always return the node, we don't want to modify it
          node))
  *diagnostics*)
