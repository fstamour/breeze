
(defpackage #:breeze.buffer
  (:documentation "Data structure to hold and manage lisp source code, its parse tree and
point.")
  (:use #:cl #:breeze.parser)
  (:import-from #:breeze.package
                #:in-package-node-p
                #:map-top-level-in-package)
  (:import-from #:alexandria
                #:if-let
                #:when-let*
                #:when-let)
  (:import-from #:breeze.queue
                #:make-queue
                #:copy-queue
                #:empty-queue-p
                #:enqueue
                #:dequeue
                #:clear-queue)
  (:import-from #:breeze.incremental-parser
                #:apply-edit-to-source)
  (:export #:buffer
           #:point
           #:name
           #:filename
           #:point-min
           #:point-max
           #:major-mode
           #:cwd
           #:in-package-nodes
           #:make-buffer
           #:parse-state
           #:update-point
           #:update-content
           #:index-in-package-nodes
           #:current-package-node
           #:current-package
           #:note-edits
           #:apply-pending-edits))

(in-package #:breeze.buffer)

(defclass buffer (named lockable)
  ((node-iterator
    :initform nil
    :initarg :node-iterator
    :accessor node-iterator
    :documentation "Node at point (iterator on the parse tree)")
   ;; TODO the docstring for current-point{,-min,-max} in command.lisp
   ;; are pretty good. Use them.
   (point
    :initform nil
    :initarg :point
    :accessor point
    :documentation "Where the point is in the buffer.")
   (filename
    :initform nil
    :initarg :filename
    :accessor filename
    :documentation "Path to the file visited (in emacs' lingo) by the buffer.")
   (point-min
    :initform nil
    :initarg :point-min
    :accessor point-min
    :documentation "If the buffer is narrowed, ... TODO")
   (point-max
    :initform nil
    :initarg :point-max
    :accessor point-max
    :documentation "If the buffer is narrowed, ... TODO")
   (major-mode
    :initform nil
    :initarg :major-mode
    :accessor major-mode
    :documentation "Mode of the buffer")
   (cwd
    :initform nil
    :initarg :cwd
    :accessor cwd
    :documentation "Current working directory of the buffer")
   (in-package-nodes
    :initform nil
    :accessor in-package-nodes
    :documentation "List of node-iterators pointing to the cl:in-package forms.")
   (pending-edits
    :initform (make-queue)
    :initarg :pending-edits
    :accessor pending-edits
    :documentation "List of edits that are to be applied to the source."))
  (:documentation "Represents an editor's buffer (e.g. an opened file)

(Technically, it represents a buffer with the mode \"lisp-mode\"..."))

(defmethod print-object ((buffer buffer) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object
        (buffer stream :type t :identity nil)
      (format stream "~s" (name buffer)))))

(defmethod clear-pending-edits ((buffer buffer))
  "Remove all pending-edits."
  (clear-queue (pending-edits buffer)))

(defmethod update-point ((buffer buffer) point)
  (when point
    (setf (point buffer) point)
    (when-let ((node-iterator (node-iterator buffer)))
      (goto-position node-iterator point))))

(defmethod source ((buffer buffer))
  (when-let ((state (parse-state buffer)))
    (source state)))

(defun %update-content (buffer new-content point)
  "Update the workspace's buffer BUFFER-NAME's content"
  (let ((old-content (source buffer)))
    (when (or (null old-content)
              (string/= old-content new-content))
      ;; parsing
      (setf (node-iterator buffer) (make-node-iterator new-content))
      ;; update some indexes
      (index-in-package-nodes buffer)
      ;; update the node-iterator based on the current point
      (update-point buffer point))))

(defmethod update-content ((buffer buffer) new-content &optional point)
  "Update the workspace's buffer BUFFER-NAME's content"
  (with-lock (buffer)
    (if new-content
        (progn
          (clear-pending-edits buffer)
          (%update-content buffer new-content point))
        (update-point buffer point)))
  ;; return the buffer
  buffer)

(defun make-buffer (&key name string point)
  (let ((buffer (make-instance 'buffer :name name :point point)))
    (when string
      (update-content buffer string point))
    buffer))

(defmethod parse-state ((buffer buffer))
  (when-let ((it (node-iterator buffer)))
    (state it)))

(defmethod make-node-iterator ((buffer buffer))
  "Make new node-iterator from BUFFER's parse-state."
  (make-node-iterator (parse-state buffer)))

(defmethod copy-iterator ((buffer buffer) &optional target)
  (copy-iterator (node-iterator buffer) target))

;; TODO add tests
(defmethod map-top-level-in-package (function (buffer buffer))
  "Map FUNCTION over all top-level (in-package ...) forms in BUFFER."
  (map-top-level-in-package function (parse-state buffer)))

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

;; TODO when in an *.asd file, fallback to :asdf-user if no in-package
;; is found
;; TODO "interpret" emacs prop lines too
(defmethod current-package-node ((buffer buffer) &optional point)
  "Find the ~(in-package ...)~ form closest to POINT in BUFFER.
Returns a parse-tree _node_, not a package."
  (let ((position (or point (point buffer)))
        (candidates (or
                     (in-package-nodes buffer)
                     (index-in-package-nodes buffer))))
    (when (and candidates (plusp (length candidates)))
      (find-if (lambda (node) (< (end node) position)) candidates :from-end t))))


;; TODO (see interactive-eval-command) - get the node, parse it, find the package
;; TODO this method is not done:
(defmethod current-package ((buffer buffer))
  "Find the ~(in-package ...)~ form closest to POINT in BUFFER and returns the _package_."
  (alexandria:when-let* (($package (current-package-node buffer))
                         (package-name (node-string-designator $package)))
    (find-package package-name)))

;; This should probably go elsewhere...
(defmethod map-top-level-forms (function (buffer buffer))
  (map-top-level-forms function (parse-state buffer)))


;;; Emacs header-line
;;
;; TODO this should be a slot on buffers
;;
;; "this won't introduce any latency /s"
#++
(progn
  (define-command header-line ()
    "Compute a string to show in emacs' header-line."
    (return-value-from-command
     (or (handler-case
             (format nil "~a ~a" (current-point)
                     (let ((node-iterator (current-node-iterator)))
                       (if node-iterator
                           (let* ((state (breeze.parser:state node-iterator))
                                  (node (breeze.iterator:value node-iterator)))
                             (breeze.parser:node-content state node))
                           "NODE-ITERATOR is nil")))
           (error (condition) (apply #'format nil (simple-condition-format-control condition)
                                     (simple-condition-format-arguments condition))))
         "An error occurred when calling breeze-header-line")))
  (export 'header-line))

(defmethod note-edits ((buffer buffer) edits
                       &aux (name (name buffer)))
  "Add applicable EDITS to the BUFFER's list of pending edits."
  (when-let ((edits (remove-if-not
                     (lambda (edit)
                       (string= (getf edit :buffer-name) name))
                     edits))
             (q (pending-edits buffer)))
    #++
    (format *trace-output* "~&edits applicable: ~s" edits)
    (with-lock (buffer)
      (dolist (edit edits)
        ;; TODO class "edit" + method "buffer-name"
        (destructuring-bind (&key buffer-name buffer-file-name
                               insert-at text
                               delete-at length
                               replace-at end)
            edit
          (declare (ignore buffer-name buffer-file-name))
          ;; edit -> (TYPE POSITION DETAIL)
          (enqueue q
                   (cond
                     (insert-at
                      (list :insert-at insert-at text))
                     (delete-at
                      (list :delete-at delete-at length))
                     (replace-at
                      (list :replace-at (cons replace-at end) text))
                     ;; TODO error?
                     (t :unknown-edit))))))))

(defmethod apply-pending-edits ((buffer buffer))
  (with-lock (buffer)
    #++
    (format *trace-output* "~&buffer ~a pending-edits: ~s"
            (name buffer)
            (car (pending-edits buffer)))
    (let ((state (parse-state buffer))
          (q (pending-edits buffer)))
      (unless (empty-queue-p q)
        (let ((q (copy-queue q)))
          (clear-queue (pending-edits buffer))
          ;; This builds a whole new string for each edits, but this
          ;; isn't as bad as it sounds because there should never be a
          ;; lot of edits at once because the editors periodically
          ;; sends the edits (after some debouncing) for linting.
          (when state
            (loop
              :for edit := (dequeue q)
              :do (apply-edit-to-source state edit)
              :until (empty-queue-p q))
            ;; parse the source again (update the parse-tree)
            ;; TODO (perf) this parse the buffer "from scratch"
            (setf (node-iterator buffer) (make-node-iterator state))
            (%update-content buffer (source state) nil)))))))
