(cl:in-package #:cl-user)

(defpackage #:breeze.parse-tree
  (:documentation "A concrete syntax tree")
  (:use #:cl
        #:breeze.generics
        #:breeze.iterator
        #:breeze.range)
  (:import-from #:breeze.parser-state
                #:state
                #:tree
                #:source-substring)
  (:import-from #:alexandria
                #:when-let
                #:when-let*
                #:if-let)
  ;; Nodes
  (:export #:+end+
           #:no-end-p
           #:range
           #:node
           #:start
           #:end
           #:add-offset
           #:node-type
           #:errors
           #:copy-node
           #:valid-node-p
           #:node-content
           #:format-error
           #:format-errors)
  ;; Node sequences
  (:export #:ensure-nodes
           #:append-nodes
           #:nodes
           #:nodesp
           #:nth-node
           #:first-node
           #:second-node
           #:last-node)
  ;; Node constructors
  (:export #:block-comment
           #:parens
           #:token
           #:ignorable-node
           #:whitespace
           #:comment
           #:line-comment
           #:string-node
           #:quote-node
           #:quasiquote
           #:dot
           #:page
           #:comma
           #:comma-at
           #:comma-dot
           #:sharp
           #:sharp-char
           #:sharp-function
           #:sharp-vector
           #:sharp-bitvector
           #:sharp-uninterned
           #:sharp-eval
           #:sharp-binary
           #:sharp-octal
           #:sharp-hexa
           #:sharp-complex
           #:sharp-structure
           #:sharp-pathname
           #:sharp-feature
           #:sharp-feature-not
           #:sharp-radix
           #:sharp-array
           #:sharp-label
           #:sharp-reference
           #:sharp-unknown
           #:shebang
           #:extraneous-closing-parens)
  ;; Node predicates
  (:export #:block-comment-node-p
           #:parens-node-p
           #:token-node-p
           #:ignorable-node-p
           #:whitespace-node-p
           #:line-comment-node-p
           #:comment-node-p
           #:whitespace-or-comment-node-p
           #:string-node-p
           #:quote-node-p
           #:quasiquote-node-p
           #:dot-node-p
           #:comma-node-p
           #:sharp-node-p
           #:sharp-char-node-p
           #:sharp-function-node-p
           #:sharp-vector-node-p
           #:sharp-bitvector-node-p
           #:sharp-uninterned-node-p
           #:sharp-eval-node-p
           #:sharp-binary-node-p
           #:sharp-octal-node-p
           #:sharp-hexa-node-p
           #:sharp-complex-node-p
           #:sharp-structure-node-p
           #:sharp-pathname-node-p
           #:sharp-feature-node-p
           #:sharp-feature-not-node-p
           #:sharp-radix-node-p
           #:sharp-array-node-p
           #:sharp-label-node-p
           #:sharp-reference-node-p
           #:sharp-unknown-node-p)
  ;; Accessors
  (:export
   #:package-prefix
   #:package-marker)
  (:export #:node-contains-position-p
           #:node-range-contains-position-p
           #:node-children-contains-position-p)
  ;; Node iterator
  (:export #:make-node-iterator
           #:node-iterator
           #:node-string
           #:goto-position
           #:parent-node
           #:root-node
           #:top-level-node-iterator
           #:map-top-level-forms))

(in-package #:breeze.parse-tree)


;;; The +end+

(alexandria:define-constant +end+ -1)

(defmethod no-end-p ((x integer))
  "Does this number represent the +infinity?"
  (= +end+ x))



(defmethod no-end-p ((range range))
  "Is this range open-ended?"
  (= +end+ (end range)))

(defmethod add-offset ((range range) offset)
  (incf (start range) offset)
  (unless (minusp (end range))
    (incf (end range) offset)))


;;; Node class

(defclass node (range)
  ((errors
    :initform nil
    :initarg :errors
    :accessor errors)))

(defmethod node-type ((node node))
  (class-name (class-of node)))

(defmethod children ((node node)))

(defun nodep (x)
  (typep x 'node))

(declaim (inline node=))
(defun node= (a b)
  (and (eq (node-type a)
           (node-type b))
       (range= a b)
       (eqv (children a) (children b))))

(defmethod eqv ((a node) (b node))
  (node= a b))

(defun valid-node-p (node)
  (and node
       (typep node 'node)
       (not (no-end-p node))
       (null (errors node))))

(defun node-content (state node)
  "Get a (displaced) string of the node's range."
  (source-substring state (start node) (end node)))

(defun ensure-nodes (x)
  "Ensure that that X is a sequence of node."
  (typecase x
    (null nil)
    (vector x)
    (cons (coerce x 'vector))
    (t (vector x))))

(defun append-nodes (nodes1 nodes2)
  "Concatenate two sequences of nodes."
  (concatenate 'vector nodes1 nodes2))

;; (declaim (inline %nodes))
(defun %nodes (x y)
  "Create a sequence of nodes. But less user-friendly."
  (if x
      (if y
          (append-nodes (ensure-nodes x) (ensure-nodes y))
          (ensure-nodes x))
      (when y (ensure-nodes y))))

(defun nodes (&optional node &rest nodes)
  "Create a sequence of nodes."
  (%nodes node nodes))

(defun nodesp (x)
  (vectorp x))

(defun nth-node (nodes n)
  (etypecase nodes
    (null nil)
    (node (nth-node (children nodes) n))
    (vector (aref nodes (if (minusp n) (+ n (length nodes)) n)))))

(defun first-node (nodes)
  (nth-node nodes 0))

(defun second-node (nodes)
  (nth-node nodes 1))

(defun last-node (nodes)
  (etypecase nodes
    (null nil)
    (node nodes)
    (vector (nth-node nodes -1))))


;;; Constructors

;; TODO
(defgeneric copy-node (node))

(defmacro define-node-type (name
                            (&key
                               (superclass 'node)
                               positional-args
                               (constructor t)
                               children)
                            &body defclass-body)
  `(breeze.class-utils:define-class
       ,name
       (:superclass ,superclass
        :positional-args ,(append
                           '(start end)
                           (if children
                               (adjoin 'children positional-args)
                               positional-args))
        :keyword-args (errors)
        :constructor ,(when constructor
                        `(:let* ((errors (if (stringp errors)
                                             (list (list errors))
                                             errors)))))
        :predicate ,(intern (breeze.string:ensure-suffixes
                             '("-NODE" "-P")
                             (symbol-name name))))
     ,@(destructuring-bind (&optional slots &rest rest)
           defclass-body
         `((,@slots
            ,@(when children
                `((children
                   :initform nil
                   :initarg :children
                   :accessor children))))
           ,@rest))))

(define-node-type ignorable-node (:constructor nil))

(define-node-type whitespace (:superclass ignorable-node))
(define-node-type page (:superclass whitespace))

(define-node-type comment (:constructor nil
                           :superclass ignorable-node))
(define-node-type block-comment (:superclass comment))
(define-node-type line-comment (:superclass comment))

(define-node-type token ()
  ((package-prefix
    :initform nil
    :initarg :package-prefix
    :accessor package-prefix
    :documentation "The package prefix of the token.")
   (package-marker
    :initform nil
    :initarg :package-marker
    :accessor package-marker
    :documentation "The package marker that separate the package prefix and the symbol's name.")
   (name
    :initform nil
    :initarg :name
    :accessor name
    :documentation "Name of the symbol.")))

(define-node-type parens (:children t))

(define-node-type string-node ())
(define-node-type quote-node (:children t))
(define-node-type quasiquote (:children t))
(define-node-type dot ())
(define-node-type comma (:children t))
(define-node-type comma-dot (:children t))
(define-node-type comma-at (:children t))


;; I don't remember this.. maybe I meant to use it for "# "?
;; (define-node-type sharp ())
(define-node-type sharp-char (:children t))
(define-node-type sharp-function (:children t))
(define-node-type sharp-vector (:children t))
(define-node-type sharp-bitvector (:children t))

;; TODO use "name" instead of children
(define-node-type sharp-uninterned (:children t))
(define-node-type sharp-eval (:children t))
(define-node-type sharp-binary (:children t))
(define-node-type sharp-octal (:children t))
(define-node-type sharp-hexa (:children t))
(define-node-type sharp-complex (:children t))
(define-node-type sharp-structure (:children t))
(define-node-type sharp-pathname (:children t))

;; TODO add a common class for "sharp-feature" and "sharp-feature-not"
(define-node-type sharp-feature (:children t))
(define-node-type sharp-feature-not (:children t))

(define-node-type sharp-radix (:children t
                               :positional-args (radix))
  ((radix
    :initform nil
    :initarg :radix
    :accessor radix
    :documentation "The radix of the #nR reader macro.")))
(define-node-type sharp-array (:children t))
(define-node-type sharp-label (:children t
                               :positional-args (label children))
  ((label
    :initform nil
    :initarg :label
    :accessor label)))

(define-node-type sharp-reference (:positional-args (label))
  ((label
    :initform nil
    :initarg :label
    :accessor label)))
(define-node-type sharp-unknown ())
(define-node-type shebang ())
(define-node-type extraneous-closing-parens ())

;; TODO use this in print-object to print children
(defun print-nodes (stream nodes colonp atp)
  (declare (ignore colonp atp))
  (format stream "(vector ~{~s~^ ~})" nodes))

(defun format-error (error stream)
  (apply #'format stream error))

(defun format-errors (node stream)
  (loop
    :with errors = (errors node)
    :for i from 0
    :for error :in errors
    :unless (zerop i)
      :do (fresh-line stream)
    :do (apply #'format stream error)))


;;; Predicates

(defun whitespace-or-comment-node-p (node)
  "Is this node a whitespace, a block comment or line comment?"
  (ignorable-node-p (etypecase node
                      ;; TODO maybe add (not (donep ...))
                      (node-iterator (value node))
                      (node node))))



;; TODO move into range.lisp
(defun node-contains-position-p (node position)
  (let ((start (start node))
        (end (end node)))
    (and (<= start position)
         (or (no-end-p node)
             (< position end)))))

;; TODO move into range.lisp
(defun node-range-contains-position-p (start-node end-node position)
  (check-type start-node node)
  (check-type end-node node)
  (let ((start (start start-node))
              (end (end end-node)))
          (and (<= start position)
               (or (no-end-p end-node)
                   (< position end)))))

(defun node-children-contains-position-p (node position)
  (when-let ((children (children node)))
    (typecase children
      (vector
       (node-range-contains-position-p
        (aref children 0)
        (aref children (1- (length children)))
        position))
      (node
       (node-contains-position-p children position)))))


;;; Node iterators

;; TODO create a mix-in class for "parser-state" (same for "name")
(defclass node-iterator (tree-iterator)
  ((parser-state
    ;; TODO rename to parser-state
    :initarg :state
    ;; :type state
    ;; TODO rename to parser-state
    :accessor state))
  (:documentation "An iterator for parse-trees."))

(defmethod copy-iterator ((node-iterator node-iterator) &optional target)
  (declare (ignore target))
  (let ((iterator (call-next-method)))
    (setf (state iterator) (state node-iterator))
    iterator))

(defmethod print-object ((node-iterator node-iterator) stream)
  (print-unreadable-object
      (node-iterator stream :type t :identity nil)
    (with-slots (depth positions subtrees) node-iterator
      (format stream "~{~s~^, ~}"
              (list depth positions)))))

(defmethod children ((iterator node-iterator))
  (unless (donep iterator)
    (children (value iterator))))

(defmethod source ((node-iterator node-iterator))
  (source (state node-iterator)))

(defmethod node-string ((_ (eql nil))))

(defmethod node-string ((node-iterator node-iterator))
  (node-content (state node-iterator) (value node-iterator)))


;;; goto point

(defmethod goto-position ((iterator node-iterator) position)
  ;; TODO this might not be necessary, as an optimization, when can
  ;; check if the current node contains the position. If yes, check if
  ;; there are children, if not, check the parent.
  ;;
  ;; TODO use binary search
  (cond
    ((<= (length (source iterator)) position)
     ;; TODO add a test for this case
     (setf position (1- (length (source iterator)))))
    ((donep iterator) (reset iterator))
    ((node-contains-position-p (root-node iterator) position)
     (loop :for d :from (slot-value iterator 'depth) :downto 0
           :if (node-contains-position-p (value-at-depth iterator d) position)
             :do (return)
           :else
             :do (pop-subtree iterator)))
    ;; TODO this could eazily be optimized by going backward (or bin search)
    ((< position (start (value iterator)))
     (reset iterator)))
  (loop
    :until (donep iterator)
    :for node = (value iterator)
    :do
    #++ (log-debug "============ depth: ~d positions: ~s donep: ~s node: ~s"
                   (slot-value iterator 'depth)
                   (slot-value iterator 'positions)
                   (donep iterator)
                   node)
        (cond
          ((= (start node) position)
           #++ (log-debug "stop: right at the start of a node")
           (return))
          ((node-contains-position-p node position)
           #++ (log-debug "node-contains-position-p ~a => T" node)
           (if (children node)
               (if (not (node-children-contains-position-p node position))
                   (progn
                     #++ (log-debug "stop: no need to recurse")
                     (return))
                   ;; this will recurse into the node's children
                   (go-down iterator))
               (progn
                 #++ (log-debug "stop: done!")
                 (return))))
          ((< position (end node))
           #++ (log-debug "stop: went too far! position: ~d node: ~s"
                          position node)
           (return))
          (t
           (next iterator))))
  iterator)



(defmethod value-at-depth ((iterator node-iterator) depth)
  (with-slots (positions subtrees) iterator
    (let ((pos (aref positions depth))
          (subtree (aref subtrees depth)))
      (etypecase subtree
        (vector (aref subtree pos))
        (t subtree)))))

(defmethod parent-node ((iterator node-iterator))
  (parent-value iterator))

(defmethod root-node ((iterator node-iterator))
  (root-value iterator))

(defmethod top-level-node-iterator ((iterator node-iterator))
  ;; TODO get the actual top-level node, not the root one (see `map-top-level-forms')
  (root iterator))

(defmethod start ((node-iterator node-iterator))
  "Get the start position of the current node."
  (start (value node-iterator)))

(defmethod end ((node-iterator node-iterator))
  "Get the end position of the current node."
  (end (value node-iterator)))




#++
(defmethod crumbs ((iterator node-iterator))
  (loop :for i :from 0 :upto (depth iterator)
        :for node-at-depth = (value-at-depth iterator i)
        :for crumb-node = (or (first-node (children node-at-depth)) node-at-depth)
        :collect (if (= i (depth iterator))
                     (node-content (state iterator) node-at-depth)
                     (when crumb-node
                       (node-content (state iterator) crumb-node)))))

#++
(defmethod crumbs ((iterator node-iterator))
  (loop :for i :from 0 :below (depth iterator)
        :for node-at-depth = (value-at-depth iterator i)
        :for crumb-node = (or (first-node (children node-at-depth))
                              node-at-depth)
        :collect (when crumb-node
                   (node-content (state iterator) crumb-node))))

#++
(let* ((input "a (b c (d (e g)))")
       (state (parse input))
       (it (make-node-iterator state)))
  (loop :for i :below (length input)
        :do (goto-position it i)
        :collect (crumbs it)))


(defun type-path (node-iterator)
  (let ((depth (slot-value node-iterator 'depth)))
    (loop :for d :upto depth
          :for node = (value-at-depth node-iterator d)
          :collect (node-type node))))

#++
(let* ((input "a (b c (d (e g)))")
       (state (parse input))
       (it (make-node-iterator state)))
  (loop :for i :below (length input)
        :do (goto-position it i)
        :collect (type-path it)))

(defmethod add-offset ((iterator node-iterator) offset)
  (loop
    :until (donep iterator)
    :for node = (value iterator)
    :do (add-offset node offset)
    (next iterator)))

;; TODO move somewhere else, this has nothing to do with "parse-tree"
(defun top-level-forms ($node)
  (breeze.pattern:let-match
      (((:either
         (progn (:maybe ?body))
         (eval-when _ (:maybe ?body))
         ((:either macrolet symbol-macrolet)
          _ #| definitions |#
          ;;
          ;; TODO this doesn't work, I think it's because (match
          ;; repetition iterator) advances the iterator when
          ;; there's no match _but_ the match is successful
          ;; because 'minimum is 0
          ;;
          ;; (:zero-or-more (declare _ #| TODO tests|#))
          (:maybe ?body))
         ;; TODO locally
         ))
       $node
       :skipp #'whitespace-or-comment-node-p)
    ?body))

;; TODO add tests
;; TODO move somewhere else, this has nothing to do with "parse-tree"
(defmethod map-top-level-forms (function (state state))
  (loop :with iterator = (make-node-iterator state)
        :until (donep iterator)
        :do (let ((node (value iterator))
                  (body (top-level-forms iterator)))
              (cond
                (body (copy-iterator body iterator))
                (t
                 (unless (whitespace-or-comment-node-p node)
                   (funcall function iterator))
                 ;; advance the iterator
                 (incf (pos iterator))
                 ;; go up if the current level is done
                 (when (next-up iterator)
                   (incf (pos iterator))))))))

;; TODO add tests
(defmethod make-node-iterator ((state state))
  (when (null (tree state))
    (error "Can't iterate on an empty parse tree."))
  (make-instance 'node-iterator
                 :root (tree state)
                 :state state))
