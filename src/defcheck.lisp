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

;; To make the transition from a bunch of functions to something more
;; structured, I'll make the "defcheck" macro compatible with "defun".
(defmacro defcheck (name lambda-list &body body)
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
      `(progn
         (defun ,name ,lambda-list ,@body)
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

(defun apply-check (check $node &key top-level-p)
  (when (and (validate-top-level-p-precondition check top-level-p)
             (validate-live-precondition check)
             (validate-node-type-precondition check $node))
    (funcall (fn check) $node)))
