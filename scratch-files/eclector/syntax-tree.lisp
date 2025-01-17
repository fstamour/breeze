(in-package #:cl-user)

(uiop:define-package #:breeze.syntax-tree
    (:documentation "Syntax tree data structure for common lisp reader")
  (:use #:cl)
  (:import-from
   #:breeze.utils
   #:symbol-package-qualified-name)
  (:export
   ;; Syntax tree types
   #:node
   #:skipped-node
   #:symbol-node
   #:read-eval-node
   #:character-node
   #:list-node
   #:function-node
   #:feature-expression-node
   #:string-node

   ;; Node accessors
   #:node-content
   #:node-prefix
   #:node-source
   #:node-raw
   #:node-start
   #:node-end
   #:node-feature-expression

   ;; Type predicates
   #:nodep
   #:skipped-node-p
   #:symbol-node-p
   #:read-eval-node-p
   #:character-node-p
   #:list-node-p
   #:function-node-p
   #:terminalp
   ;; TODO feature-expression, string-node

   #:find-path-to-node
   #:find-nearest-sibling-form
   #:find-nearest-sibling-in-package-form
   #:in-package-node-package
   #:nodes-emptyp
   #:loop-form-p
   #:defpackage-form-p
   #:mapcar-form-p

   #:define-node-form-predicates

   ;; Functions to extract information from specific forms
   #:in-package-node-package))

(in-package #:breeze.syntax-tree)


;;; Syntax tree data structure

(defclass node ()
  ((content
    :initform nil
    :initarg :content
    :accessor node-content)
   (prefix
    :initform nil
    :initarg :prefix
    :accessor node-prefix)
   (source
    :initform nil
    :initarg :source
    :accessor node-source)
   (raw
    :initform nil
    :initarg :raw
    :accessor node-raw))
  (:documentation "Base class for the parse-results (syntax tree)."))

(defun node-start (node)
  "Return the position where NODE starts."
  (car (node-source node)))

(defun node-end (node)
  "Return the positiion where NODE ends."
  (cdr (node-source node)))

(defclass skipped-node (node)
  ()
  (:documentation "Syntax node for skipped content."))

(defclass symbol-node (node)
  ()
  (:documentation "Syntax node for symbols."))

(defclass string-node (node)
  ()
  (:documentation "Syntax node for strings."))

(defclass read-eval-node (node)
  ()
  (:documentation "Syntax node for #. (read-eval)."))

(defclass character-node (node)
  ((char
    :initform nil
    :initarg :char
    :accessor node-char))
  (:documentation "Syntax node for #\\ (character literals)."))

(defclass list-node (node)
  ()
  (:documentation "Syntax node for lists."))

(defclass feature-expression-node (node)
  ((feature-expression
    :initform nil
    :initarg :feature-expression
    :accessor node-feature-expression))
  (:documentation "Syntax node for a feature expression."))

(defclass function-node (node)
  ()
  (:documentation "Syntax node for #'expression."))

;; TODO Use this, for better tracing
(defun make-node (type &rest args)
  (apply #'make-instance type args))


;;; Printing

(defun cropped (string &optional (length 25))
  (str:replace-all #.(coerce (list #\Newline) 'string) "\\n"
                   (str:shorten length string)))

(defun print-node-type (node stream)
  (format stream "~@(~a~) "
          (if (eq 'node (type-of node))
              "Node"
              (let ((type (symbol-name (type-of node))))
                (subseq type 0 (- (length type) #. (length "-node")))
                ;; (subseq type 0 3)
                ))))

(defun print-node-prefix (node stream)
  (alexandria:if-let (prefix (node-prefix node))
    (format stream "~s "
            (if (every #'breeze.string:whitespacep prefix)
                (length prefix)
                (node-prefix node)))))

(defmacro print-node (&body body)
  `(let ((*print-circle* t)
         (*print-right-margin* nil))
     (print-unreadable-object
         (node stream)
       ,@body)))

(defmethod print-object ((node node) stream)
  (print-node
    (print-node-type node stream)
    (print-node-prefix node stream)
    (format stream "<~a>~%~:t ~s"
            (cropped
             (node-raw node))
            (node-content node))))

(defmethod print-object ((node symbol-node) stream)
  (print-node
    (print-node-type node stream)
    (format stream "~s " (symbol-package (node-content node)))
    (print-node-prefix node stream)
    (format stream "~s"
            (or (node-raw node) (node-content node)))))

(defmethod print-object ((node skipped-node) stream)
  (print-node
    (print-node-type node stream)
    (print-node-prefix node stream)
    ;; TODO Only print the first 15 characters
    (format stream "~s"
            (node-content node))))

(defmethod print-object ((node feature-expression-node) stream)
  (print-node
    (print-node-type node stream)
    (print-node-prefix node stream)
    (format stream "~s ~s :raw ~s"
            (node-feature-expression node)
            (node-content node)
            (node-raw node))))


;;; Type predicates

(defmacro define-node-type-predicates (types)
  `(progn
     ,@(loop :for type :in types
             :collect
             `(defun ,(alexandria:symbolicate
                       type
                       (if (position #\- (symbol-name type))
                           '-p
                           'p))
                  (node)
                (typep node ',type)))))

(define-node-type-predicates
    (node
     skipped-node
     symbol-node
     read-eval-node
     character-node
     list-node
     function-node))

(defgeneric non-terminal-p (node)
  (:documentation "Can a node contain other nodes.")
  (:method ((node node)) (listp (node-content node)))
  (:method ((node skipped-node)) nil)
  (:method ((node symbol-node)) nil)
  (:method ((node character-node)) nil))

(defgeneric terminalp (node)
  (:documentation "Can a node contain other nodes.")
  (:method ((node node)) (not (non-terminal-p node))))




;;; Utility function

;; TODO node-symbol-qualified-p : is the symbol "package-qualified"

(defun node-first (node)
  "Return the first element from a NODE's content."
  (first (node-content node)))

(defun node-lastcar (node)
  "Return the last element from a NODE's content."
  (alexandria:lastcar (node-content node)))

(defun node-string-equal (string node)
  "Compare the content of a NODE to a STRING, case-insensitive."
  (string-equal string (node-content node)))

(defun node-length (node &optional (ignore-skipped-p t))
  "Returns the length of a NODE's content."
  (and (list-node-p node)
       (length
        (if ignore-skipped-p
            (remove-if #'skipped-node-p (node-content node))
            (node-content node)))))

;; TODO This does not take the symbol's package into account.
;; TODO This assumes the client can pass a symbol (which is true for
;; symbols from cl, but no other package, in general.
(defun node-symbol= (symbol node)
  "Does NODE represent the symbol SYMBOL."
  (and (symbol-node-p node)
       (node-string-equal (symbol-name symbol)
                          node)))

(defun null-node-p (node)
  "Does NODE represent the symbol \"nil\"."
  (node-symbol= 'nil node))

;; TODO A "skipped node" can also be a form hidden behind a feature (#+/-)
(defun nodes-emptyp (nodes)
  "Whether a list of node contains no code."
  (every #'(lambda (node)
             (typep node 'skipped-node))
         nodes))

(defun list-car-symbol= (node car-symbol-name)
  (and (list-node-p node)
       (node-symbol= car-symbol-name (node-first node))))

(defmacro define-node-form-predicates (types)
  "Helper macro to define lots of predicates."
  `(progn
     ,@(loop :for type :in types
             :for package = (symbol-package type)
             :for cl-package-p = (eq (find-package :cl) package)
             :for function-name
               = (if cl-package-p
                     (alexandria:symbolicate type '#:-form-p)
                     (alexandria:symbolicate (package-name package)
                                             '#:-- type '#:-form-p))
             :collect
             `(export
               (defun ,function-name
                   (node)
                 ,(format nil "Does NODE represent an \"~a\" form."
                          (if cl-package-p
                              type
                              (symbol-package-qualified-name type)))
                 (and (list-node-p node)
                      (node-symbol= ',type (node-first node))))))))

(defun find-node (position nodes)
  "Given a list of NODES, return which node contains the POSITION."
  (check-type nodes list)
  (loop :for node :in nodes
        :for (start . end) = (node-source node)
        :for i :from 0
        :when (and
               (<= start position end)
               (< position end))
          :do
             (return (cons node i))))

(defun find-path-to-node (position nodes)
  "Given a list of NODES, return a path (list of cons (node . index))"
  (check-type nodes list)
  (loop :for found = (find-node position nodes)
          :then (let ((node (car found)))
                  (and (listp (node-content node))
                       (car (node-content node))
                       (find-node position (node-content node))))
        :while found
        :collect found))

(defun find-nearest-sibling-form (nodes position-or-node predicate)
  (check-type nodes list)
  (check-type position-or-node (or node integer))
  "Find the nearest sibling form that match the predicate."
  (loop :with result
        :with position = (if (integerp position-or-node)
                             position-or-node
                             (node-start position-or-node))
        :for node :in nodes
        :when (<= position (node-start node))
          :do (return result)
        :when (funcall predicate node)
          :do (setf result node)))

(defmacro define-find-nearest-sibling-form (types)
  "Helper macro to define lots of predicates."
  `(progn
     ,@(loop :for type :in types
             :collect
             `(export
               (defun ,(alexandria:symbolicate '#:find-nearest-sibling- type
                                               '#:-form)
                   (nodes current-node)
                 ,(format
                   nil "Find the nearest sibling form of type \"~a\"."
                   type)
                 (find-nearest-sibling-form
                  nodes current-node
                  #',(alexandria:symbolicate type '#:-form-p)))))))

(defun find-nearest-parent-form (path predicate)
  "Find the nearest parent form that match the predicate."
  (loop :with result
        :for node :in path
        :when (funcall predicate node)
          :do (setf result node)
        :finally (return result)))


(defmacro define-find-nearest-parent-form (types)
  "Helper macro to define lots of predicates."
  `(progn
     ,@(loop :for type :in types
             :collect
             `(export
               (defun ,(alexandria:symbolicate '#:find-nearest-parent- type
                                               '#:-form)
                   (path)
                 ,(format
                   nil "Find the nearest parent form of type \"~a\"."
                   type)
                 (find-nearest-parent-form
                  path
                  #',(alexandria:symbolicate type '#:-form-p)))))))



(defmacro define-node-utilities (types)
  "Helper macro to define lots of predicates."
  `(progn
     (define-node-form-predicates ,types)
     (define-find-nearest-sibling-form ,types)
     (define-find-nearest-parent-form ,types)))

(define-node-utilities
    (if
     when unless
     defpackage
     in-package
     defparameter
     defvar
     loop
     defun
     defmacro
     defmethod
     defgeneric
     defconstant
     defclass
     let
     flet
     labels
     lambda
     map
     mapcar
     mapcon))

(defun in-package-node-package (in-package-node)
  "Get the package-designator out of a \"cl:in-package\" node."
  (node-content
   (second (node-content in-package-node))))
