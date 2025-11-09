(in-package #:breeze.pattern)


;;; Bindings (e.g. the result of a successful match)

(defclass binding ()
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to)
   ;; TODO source/location: optional slot to register where this
   ;; information came from. This would come in handy for error
   ;; messages; especially if we try to use the patterns for
   ;; unification, and use unification for type checking.
   (pattern
    :initform nil
    :initarg :pattern
    :reader pattern
    :documentation "The pattern object from which the binding originates.")
   (children
    :initform nil
    :initarg :children
    :reader children
    :documentation "The pattern's sub-patterns bindings."))
  (:documentation "A binding"))

(defclass multi-valued-binding (binding)
  ()
  (:documentation "A binding that is expected to bind to a set of values (i.e. the `to'
slot should be a vector)."))

(defun make-binding (from to &key pattern children)
  (make-instance
   'binding
   :from from
   :to (if (and (varp from) (multi-valued-p from))
           (vector to)
           to)
   :pattern pattern :children children))

(defun bindingp (x)
  (typep x 'binding))

(defmethod print-object ((binding binding) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object
        (binding stream :type t)
      ;; TODO prin
      (format stream "~s → ~s" (from binding) (to binding)))))

(defmethod to ((_ null)))
(defmethod from ((_ null)))

(defmethod eqv ((a binding) (b binding))
  ;; TODO
  (and (eqv (from a) (from b))
       (eqv (to a) (to b))))

(defclass substitutions ()
  ((bindings
    :initform (make-hash-table)
    :initarg :bindings
    :reader bindings))
  ;; TODO add a union-hash to detect cycles...
  (:documentation "A set of bindings"))

(defun substitutions-p (x)
  (or (eq x t)
      (typep x 'substitutions)))

(defmethod print-object ((substitutions substitutions) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object
        (substitutions stream :type t)
      (let* ((bindings (bindings substitutions))
             (size (hash-table-count bindings)))
        (cond
          ((zerop size) (write-string "(empty)" stream))
          ((= 1 size) (prin1 (alexandria:hash-table-alist bindings) stream))
          (t (format stream "(~d bindings)" size)))))))

(defmethod eqv ((a substitutions) (b substitutions))
  (eqv (bindings a) (bindings b)))

(defmethod emptyp ((substitutions substitutions))
  (zerop (hash-table-count (bindings substitutions))))

;; I think using the convention of "T represents an empty binding set,
;; which represents a successful match without captures" is nice, the
;; GC might like it too...
(defmethod emptyp ((substitutions (eql t)))
  t)

(defmethod emptyp ((binding binding))
  nil)

(defun make-substitutions (&key bindings)
  (if bindings
    (make-instance 'substitutions :bindings bindings)
    (make-instance 'substitutions)))

(defun copy-substitutions (substitutions)
  (make-substitutions
   :bindings (alexandria:copy-hash-table (bindings substitutions))))

;; TODO maybe this could be a method instead of a defun?
(defun find-binding (substitutions from)
  (etypecase substitutions
    ((or (eql t) (eql nil)) nil)
    (binding (when (eq from (from substitutions))
               substitutions))
    (substitutions
     (gethash from
              (bindings substitutions)))))

;; TODO maybe this could be a method instead of a defun?
(defun set-binding (substitutions binding)
  (setf (gethash (from binding) (bindings substitutions)) binding))

(defmethod add-binding ((substitutions substitutions) (_ (eql t)))
  ;; nothing to do
  t ; success
  )

(defmethod add-binding ((substitutions substitutions) (_ (eql nil)))
  ;; nothing to do
  nil ; failure
  )

(defmethod add-binding ((substitutions substitutions) (new-binding binding))
  (let ((old-binding (find-binding substitutions (from new-binding))))
    (and (if old-binding
             ;; (error "Conflicting bindings: ~a ~a" a b)
               ;; TODO
             (eql (to old-binding) (to new-binding))
             (set-binding substitutions new-binding))
         substitutions)))

(defun ensure-substitutions (x)
  (etypecase x
    (substitutions x)
    (binding (let ((substitutions (make-substitutions)))
               (add-binding substitutions x)
               substitutions))
    (null nil)
    ((eql t) t)))

  ;; TODO
(defun merge-substitutions (bindings1 bindings2)
  (cond
    ((or (null bindings1) (null bindings2)) nil)
    ((and (eq t bindings1) (eq t bindings2)) t)
    ;; when merging two binding instances instead of substitutionss
    ((and (bindingp bindings1) (bindingp bindings2))
     (let ((result (make-substitutions)))
       (add-binding result bindings1)
       (when (add-binding result bindings2)
         result)))
    ((emptyp bindings1) (ensure-substitutions bindings2))
    ((emptyp bindings2) (ensure-substitutions bindings1))
    ;; 1 is instance, 2 is set
    ((bindingp bindings1)
     (add-binding bindings2 bindings1))
    ;; 2 is instance, 1 is set
    ((bindingp bindings2)
     (add-binding bindings1 bindings2))
    (t
     ;; TODO It would be possible to pass the bindings into all "match"
     ;; functions and methods. It would allow to detect conflicting
     ;; bindings earlier and stop the matching process earlier.
     ;;
     ;; N.B. a disjoint-set data structure could help detect cycles in
     ;; the substitutions.
     ;;
     ;; TODO this might be faster if it merged the common bindings first, but I'm not sure how many bindings must there be to be worth it.
     ;;
     ;; TODO another perf heuristic: copy the smallest or biggest
     ;; substitutions. copying the smallest would be faster, but it
     ;; implies that `add-bindings'will be called more times.
     (let ((result (copy-substitutions bindings1)))
       (loop :for from2 :being :the :hash-key :of (bindings bindings2) :using (hash-value binding2)
             :for successp = (add-binding result binding2)
             :unless successp :do (return))
       result))))

(defun merge-sets-of-substitutions (set-of-substitutions1 set-of-substitutions2)
  "Merge two set (list) of `substitutions', returns a new set of
`substitutions'.

Matching a pattern against a set of values (e.g. an egraph) will yield
a set of independent substitutions. During the matching process, we
might need to refine the \"current\" set of \"partial\"
`substitutions'.  This is done by computing the Cartesian product of
the two sets of substitutions and keeping only the resulting
substitutions that don't have any conflicting bindings."
  (loop
    :for substitutions1 :in set-of-substitutions1
    :append (loop
              :for substitutions2 :in set-of-substitutions2
              :for merged-substitutions = (merge-substitutions
                                      substitutions1 substitutions2)
              :when merged-substitutions
                :collect merged-substitutions)))
