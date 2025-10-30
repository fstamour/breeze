
(defun null-node-p (node)
  "Does NODE represent the symbol \"nil\"."
  (node-symbol= 'nil node))

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
