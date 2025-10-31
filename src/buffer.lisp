
(defpackage #:breeze.buffer
  (:documentation "Data structure to hold and manage lisp source code, its parse tree and
point.")
  (:use #:cl #:breeze.parser)
  (:import-from #:breeze.generics
                #:name)
  (:import-from #:breeze.package
                #:in-package-node-p
                #:map-top-level-in-package)
  (:import-from #:alexandria
                #:if-let
                #:when-let*
                #:when-let)
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
           #:update-content
           #:index-in-package-nodes
           #:current-package))

(in-package #:breeze.buffer)

(defclass buffer ()
  (#++ ;; TODO
   (editor
    :initform nil
    :initarg :editor
    :accessor editor
    :documentation "In which editor is this buffer opened?")
   (name
    :initform nil
    :initarg :name
    :accessor name
    :documentation "Name of the buffer")
   (node-iterator
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
    :documentation "List of node-iterators pointing to the cl:in-package forms."))
  (:documentation "Represents an editor's buffer (e.g. an opened file)

(Technically, it represents a buffer with the mode \"lisp-mode\"..."))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object
      (buffer stream :type t :identity nil)
    (format stream "~s" (name buffer))))

(defmethod update-point ((buffer buffer) point)
  (when point
    (setf (point buffer) point)
    (when-let ((node-iterator (node-iterator buffer)))
      (goto-position node-iterator point))))

(defmethod source ((buffer buffer))
  (when-let ((state (parse-state buffer)))
    (source state)))

(defmethod update-content ((buffer buffer) new-content &optional point)
  "Update the workspace's buffer BUFFER-NAME's content"
  (when new-content
    (let ((old-content (source buffer)))
      (cond
        ((and old-content
              (string/= old-content new-content))
         (breeze.logging:log-debug "re-parsing the buffer ~s from scratch" (name buffer))
         (setf (node-iterator buffer) (make-node-iterator new-content)))
        ((null old-content)
         (breeze.logging:log-debug "parsing the buffer ~s for the first time" (name buffer))
         (setf (node-iterator buffer) (make-node-iterator new-content)))))
    ;; update some indexes
    (index-in-package-nodes buffer))
  (update-point buffer point)
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

;; TODO rename to current-package-node
(defmethod current-package ((buffer buffer) &optional point)
  (let ((position (or point (point buffer)))
        (candidates (or
                     (in-package-nodes buffer)
                     (index-in-package-nodes buffer))))
    (when (and candidates (plusp (length candidates)))
      (find-if (lambda (node) (< (end node) position)) candidates :from-end t))))


;; TODO (see interactive-eval-command) - get the node, parse it, find the package
;; (defmethod current-package ((buffer buffer)))

(defmethod map-top-level-forms (function (buffer buffer))
  (map-top-level-forms function (parse-state buffer)))
