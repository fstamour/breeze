
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
   #:lint
   #:fix))

(in-package #:breeze.analysis)


;;; Basic utilities for nodes

(defun node-length (node)
  "Returns the number of children of NODE, or nil if it doesn't have any
children nodes."
  (let ((children (node-children node)))
    (when (listp children)
      (length children))))

(defun node-string= (state node string)
  (string= (source state) string
           :start1 (node-start node)
           :end1 (node-end node)))

(defun node-string-equal (state node string)
  (string-equal (source state) string
                :start1 (node-start node)
                :end1 (node-end node)))


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

(defun match-symbol-to-token (symbol token-node &aux (state *state*))
  (and
   (symbolp symbol)
   (token-node-p token-node)
   (let* ((name (symbol-name symbol))
          (package (symbol-package symbol))
          ;; TODO would be nice to cache this
          (symbol-node (token-symbol-node state token-node)))
     ;; TODO use case-sensitive comparison, but convert case if
     ;; necessary (i.e. depending on *read-case*)
     (when symbol-node
       (and
        (ecase (node-type symbol-node)
          (current-package-symbol (node-string-equal state token-node name))
          (keyword
           (and (string-equal "KEYWORD" (package-name package))
                (node-string-equal state symbol-node name)))
          (uninterned-symbol
           (and (null package)
                (node-string-equal state symbol-node name)))
          ((qualified-symbol possibly-internal-symbol)
           (destructuring-bind (package-name-node symbol-name-node)
               (node-children symbol-node)
             (and
              (node-string-equal state symbol-name-node name)
              (some (lambda (package-name)
                      (node-string-equal state package-name-node package-name))
                    `(,(package-name package)
                      ,@(package-nicknames package)))))))
        ;; symbol-node
        t)))))

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

;; TODO I want to check if a node is an "in-package" node...
;; - case converting if necessary
;; - skip whitespaces
;; - check if there's a package designator
;;
;; Now, I have a chicken-and-egg issue because of
;; package-local-nicknames...  I need to know what is the current
;; package to look for PLNs to find the in-pacakge form, but I need
;; the in-package to know the current package.

(defmacro define-node-matcher (name (pattern) &body body)
  `(defun ,name (state node)
     ,(format nil "Does NODE match ~s?" pattern)
     (let* ((*state* state)
            (*match-skip* #'whitespace-or-comment-node-p)
            (bindings (match (compile-pattern ,pattern) node)))
       ,@body)))

#++ ;; TODO
(define-node-matcher in-package-node-p ('(in-package :?package-designator))
  (when bindings
    (second bindings)
    #++
    (destructuring-bind (&key ?package-designator) bindings
      ?package-designator)))

(defun in-package-node-p (state node)
  "Is NODE a cl:in-package node?"
  (let* ((*state* state)
         (*match-skip* #'whitespace-or-comment-node-p)
         (bindings (match #.(compile-pattern `(in-package :?package)) node))
         (package-designator-node (cdr (find-binding bindings :?package))))
    package-designator-node))

#++ (compile-pattern '(if :?cond :?then :?else :?extra (:zero-or-more :?extras)))

(define-node-matcher malformed-if-node-p ('(if :?cond :?then :?else :?extra (:zero-or-more :?extras)))
  (when bindings
    ;; (destructuring-bind (&key ?cond ?then ?else ?extra) bindings)
    t))

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


;; TODO try to keep track whether the current node is quoted or not
(defun %walk (state callback tree depth quotedp)
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
                               :previous previous
                               :quotedp quotedp)
                           (1+ depth)
                           quotedp)))
        (node
         (case (node-type tree)
           (parens
            (cb tree :beforep t :quotedp quotedp)
            (%walk state
                   callback
                   (node-children tree)
                   (1+ depth)
                   quotedp)
            (cb tree :afterp t :quotedp quotedp))
           (t
            (cb tree))))))))

(defun walk (state callback &optional quotedp)
  "Call CALLBACK over all nodes in the parse tree contained by STATE.
CALLBACK will be called multiple times on the same node, with
different parameters.

When CALLBACK is called with :aroundp t, the CALLBACK can decide to
stop the walk here(i.e. not recurse) by returning nil. The CALLBACK
can also return a new node altogether, the walk will
continue (recurse) in this new node instead.

"
  (%walk state callback (tree state) 0 quotedp))

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
  (push-diagnostic* (node-start node) (node-end node)
                    severity format-string format-args))

(defun diag-warn (node format-string &rest format-args)
  (apply #'diag-node node :warning format-string format-args))

(defun diag-error (node format-string &rest format-args)
  (apply #'diag-node node :error format-string format-args))



(defun warn-undefined-in-package (state node)
  (alexandria:when-let ((package-designator-node (in-package-node-p state node)))

    (let* ((package-designator (read-from-string (node-content state package-designator-node))))
      (when (and (typep package-designator 'breeze.utils:string-designator)
                 (null (find-package package-designator)))
        (diag-warn
         node
         "Package ~s is not currently defined." package-designator)))))
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
  ((node :initarg :node :reader target-node)
   (replacement :initarg :replacement :reader replacement :initform 'cl:null)))

(defun replacementp (simple-node-condition)
  (not (eq (replacement simple-node-condition) 'cl:null)))

(define-condition simple-node-error (simple-node-condition simple-error) ())

(define-condition simple-node-warning (simple-node-condition simple-warning) ())

(define-condition node-parse-error (simple-node-error parse-error) ())

(define-condition node-style-warning (simple-node-warning style-warning) ())

(defun node-style-warning (node message &key (replacement 'null))
  (signal (make-condition
           'node-style-warning
           :node node
           :format-control message
           :replacement replacement)))

#++
(let ((c (node-style-warning (node 'dummy 0 1) "Bad node ~a")))
  (apply #'format nil (simple-condition-format-control c)
         (target-node c)
         (simple-condition-format-arguments c)))

(defun warn-extraneous-whitespaces (state node firstp lastp previous)
  (cond
    ((and firstp lastp)
     (node-style-warning
      node "Extraneous whitespaces."
      :replacement nil))
    (firstp
     (node-style-warning
      node "Extraneous leading whitespaces."
      :replacement nil))
    ((and lastp (not (line-comment-node-p previous)))
     (node-style-warning
      node "Extraneous trailing whitespaces."
      :replacement nil))
    ((and (not (or firstp lastp))
          ;; Longer than 1
          (< 1 (- (node-end node) (node-start node)))
          ;; "contains no newline"
          (not (position #\Newline
                         (source state)
                         :start (node-start node)
                         :end (node-end node))))
     (node-style-warning
      node "Extraneous internal whitespaces."
      :replacement " "))))

(defun error-invalid-node (node)
  (unless (valid-node-p node)
    (diag-error node "Syntax error")))

(defun analyse (&key buffer-string point-max callback
                &allow-other-keys
                &aux
                  (state (parse buffer-string))
                  (*point-max* (or point-max (length buffer-string))))
  (walk state
        (lambda (node &rest args &key depth aroundp beforep afterp
                                   firstp lastp nth
                                   previous
                                   quotedp
                 &allow-other-keys)
          (declare (ignorable depth beforep afterp nth args quotedp))
          (let ((new-node
                  (catch 'return-node
                    ;; Debug info
                    ;; (format *debug-io* "~&~s ~{~s~^ ~}" node args)
                    (when aroundp
                      (error-invalid-node node)
                      (warn-undefined-in-package state node)
                      (when (and (plusp depth)
                                 (whitespace-node-p node))
                        (warn-extraneous-whitespaces state node firstp lastp previous)))
                    ;; Always return the node, we don't want to modify it.
                    ;; Technically, we could return nil to avoid recursing into
                    ;; the node (when aroundp is true, that is).
                    node)))
            (if callback
                (apply callback new-node :state state #| 👀 |#args)
                new-node)))))

(defun lint (&key buffer-string point-max
             &allow-other-keys
             &aux (*diagnostics* '()))
  (handler-bind
      ((node-style-warning (lambda (condition)
                             (format *debug-io* "NODE-STYLE-WARNING")
                             (diag-warn (target-node condition)
                                        (simple-condition-format-control condition)
                                        (simple-condition-format-arguments condition)))))
    (analyse :buffer-string buffer-string
             :point-max point-max))
  *diagnostics*)

(defun fix (&key buffer-string point-max &allow-other-keys
            &aux fixed-anything-p)
  (values (with-output-to-string (out)
            (handler-bind
                ((node-style-warning (lambda (condition)
                                       (when (replacementp condition)
                                         (setf fixed-anything-p t)
                                         (let ((replacement (replacement condition)))
                                           ;; (format *debug-io* "~&got the condition: ~a ~s" condition replacement)
                                           (when (stringp replacement)
                                             (write-string (replacement condition) out)))
                                         (throw 'return-node nil)))))
              (analyse :buffer-string buffer-string
                       :point-max point-max
                       :callback (lambda (node &rest args &key
                                                            state
                                                            depth aroundp beforep afterp
                                                            firstp lastp nth
                                                            previous
                                                            quotedp
                                          &allow-other-keys)
                                   ;; (format *debug-io* "~&~s ~{~s~^ ~}" node args)
                                   (when (and (not (valid-node-p node))
                                              (parens-node-p node))
                                     (setf fixed-anything-p t))
                                   (cond
                                     (beforep
                                      (write-char #\( out))
                                     (afterp
                                      (write-char #\) out))
                                     ((not (or aroundp beforep afterp))
                                      (breeze.lossless-reader::write-node node state out)))
                                   node))))
          fixed-anything-p))
