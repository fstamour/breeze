(defpackage #:breeze.checks
  (:documentation "Utilities to define linter \"checks\".

They are not called rules to avoid confusion with the rewrite rules.")
  (:use #:cl)
  (:export #:defcheck
           #:apply-check
           #:*checks*
           #:top-level-checks
           #:apply-check))

(in-package #:breeze.checks)


;;; Checks (linter rules)

(defclass check ()
  ((fn
    :initform nil
    :initarg :fn
    :accessor fn
    :documentation "The function to call to run this check.")
   ;; aka selector?
   (preconditions
    :initform nil
    :initarg :preconditions
    :accessor preconditions
    :documentation "List of conditions in which this check is applicable.")
   ;; TODO categories (tags?)
   ;; TODO doc url??
   )
  (:documentation "Represent a linter rule."))

(defvar *checks* (make-hash-table)
  "Set of all the checks the linter can run.")

(defmethod initialize-instance :after ((check check) &key &allow-other-keys)
  (setf (gethash (fn check) *checks*) check))

(defun ensure-allow-other-keys (lambda-list)
  (cond
    ((member '&allow-other-keys lambda-list) lambda-list)
    ((member '&key lambda-list) `(,@lambda-list &allow-other-keys))
    (t `(,@lambda-list &key &allow-other-keys))))

;; TODO add a restart (in defcheck) to disable a check

;; To make the transition from a bunch of functions to something more
;; structured, I'll make the "defcheck" macro compatible with "defun".
(defmacro defcheck (name lambda-list &body body)
  "Define a linter check.

`defcheck' has the same syntax as `cl:defun' with one extra syntax:
before the documentation string and the declaration, `defcheck' will
look for a plist that will be used to specify preconditions that must
be satisfied to run this check.

The current supported preconditions are:

:node-type <type>

  the check will only be called on node of that type

  for example:
    :node-type breeze.parse-tree:token

:top-level-p <boolean>

  when T, the check will only be applied to nodes corresponding to
  top-level forms. Beware, there is a distinction between root-level
  and top-level: root-level means there are no parent, top-level forms
  can be root-level, or subforms of `progn', `locally', etc. (see
  https://www.lispworks.com/documentation/HyperSpec/Body/03_bca.htm).

:livep <boolean>

  Whether this check only make sense on a \"live\" image, as opposed
  to running the linter as batch process. This can be useful to add
  checks that warns about things that are not defined in the current
  image, this can help keeping the code and the image in sync.

Example:

(defcheck warn-about-long-name (node-iterator)
  :node-type 'token
  (let ((token (value node-iterator)))
    (when (< 42 (length (name token)))
      ;; this will signal a 'node-style-warning condition
      (node-style-warning
       node-iterator \"Token length is greater than 42.\"))))

The check is automatically added to the hash-table
breeze.checks::*checks*, the name (symbol) is used as the key and an
object of they breeze.check:check is used as the value. The object
holds the function to run to apply the chekc and a list of
preconditions that defines where/when this check is applicable."
  ;; TODO docstring for defcheck
  (multiple-value-bind (preconditions body)
      (loop
        :for rest :on body :by #'cddr
        :for (k v) := rest
        :while (keywordp k)
        :append (list k v) :into plist
        :finally (return (values plist rest)))
    ;; TODO validate preconditions
    ;;  - supported: :node-type, :top-level-p and :livep
    ;;  - :node-type's value should be a symbol
    (multiple-value-bind (remaining-forms declarations docstring)
        (alexandria:parse-body body :documentation t)
      (declare (ignore remaining-forms declarations))
      (declare (ignorable docstring))
      ;; TODO No check have a docstring at the moment, just skip that
      ;; validation for now
      #++
      (unless docstring
        (warn "The check ~s doesn't have a docstring" name))
      #++
      (check-type docstring string
                  "Docstring are mandatory for checks")
      (unless preconditions
        (error "For performance reasons, a check should have at least one precondition."))
      (when (and (getf preconditions :top-level-p)
                 (getf preconditions :node-type))
        (error "For now, as an optimisation, a check cannot have both :top-level-p and :node-type"))
      `(progn
         (defun ,name ,(ensure-allow-other-keys lambda-list) ,@body)
         (make-instance 'check
                        :fn ',name
                        :preconditions ',preconditions)))))

(defun validate-live-precondition (check)
  (if (getf (preconditions check) :livep)
      (breeze.diagnostics:livep*)
      ;; the check does not have the "livep" precondition, so it
      ;; should run regardless of whether we're in a live session or
      ;; not.
      t))

(defun validate-node-type-precondition (check $node)
  (let ((required-node-type (getf (preconditions check) :node-type)))
    (if required-node-type
        (let* ((actual-node-type (breeze.parser:node-type
                                  (breeze.iterator:value $node)))
               (invertedp (and (listp required-node-type)
                                          (eq :not (car required-node-type))))
               (required-node-type (if invertedp
                                     (second required-node-type)
                                     required-node-type)))
          (unless actual-node-type
            (error "Something went wrong when validating a check's :node-type precondition."))
          (if invertedp
              (not (eq required-node-type actual-node-type))
              (eq required-node-type actual-node-type)))
        ;; the check does not have the "node-type" preconditions, so
        ;; it should run regardless of the $node's type.
        t)))

(defun validate-top-level-p-precondition (check top-level-p)
  (if (getf (preconditions check) :top-level-p)
      top-level-p
      t))

(defun apply-check (check buffer $node &key top-level-p)
  (when (and (validate-top-level-p-precondition check top-level-p)
             (validate-live-precondition check)
             (validate-node-type-precondition check $node))
    (funcall (fn check) $node :buffer buffer)))



(defun filter-checks-by-livep (list-of-checks)
  (if (breeze.diagnostics:livep*)
      list-of-checks
      (loop :for check :in list-of-checks
            :unless (getf (preconditions check) :livep)
              :collect check)))

(defun group-checks (checks)
  (flet ((splat (check)
           (destructuring-bind (&key livep top-level-p node-type)
               (preconditions check)
             (declare (ignore livep))
             (list top-level-p node-type))))
    (loop
      :with by-node-types := (make-hash-table)
      :with by-not-node-types := (make-hash-table)
      :for check :in (filter-checks-by-livep (alexandria:hash-table-values checks))
      :for (top-level-p node-type) := (splat check)
      :when top-level-p :collect (fn check) :into top-level-checks
        :when node-type :do
          (if (and (listp node-type)
                   (eq :not (first node-type)))
              (push (fn check) (gethash (second node-type) by-not-node-types))
              (push (fn check) (gethash node-type by-node-types)))
          :finally (return (list :top-level-checks top-level-checks
                                 :by-node-types by-node-types
                                 :by-not-node-types by-not-node-types)))))
