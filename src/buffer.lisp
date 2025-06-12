
(defpackage #:breeze.buffer
  (:documentation "")
  (:use #:cl #:breeze.lossless-reader)
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
           #:in-package-nodes
           #:make-buffer
           #:update-buffer-content
           #:index-in-package-nodes))

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
   (in-package-nodes
    :initform nil
    :accessor in-package-nodes
    :documentation "List of node-iterators pointing to the cl:in-package forms."))
  (:documentation "Represents an editor's buffer (e.g. an opened file)

(Technically, it represents a buffer with the mode \"lisp-mode\"..."))

(defmethod update-buffer-content ((buffer buffer) new-content)
  "Update the workspace's buffer BUFFER-NAME's content"
  (when new-content
    (if-let ((old-node-iterator (node-iterator buffer)))
      (unless (string= (source (state old-node-iterator)) new-content)
        (breeze.logging:log-debug "re-parsing the buffer ~s from scratch" (name buffer))
        (setf (node-iterator buffer) (make-node-iterator new-content)))
      (progn (breeze.logging:log-debug "parsing the buffer ~s for the first time" (name buffer))
             (setf (node-iterator buffer) (make-node-iterator new-content))))
    (index-in-package-nodes buffer))
  buffer)

(defun make-buffer (&key name string)
  (let ((buffer (make-instance 'buffer :name name)))
    (when string
      (update-buffer-content buffer string))
    buffer))

(defmethod parse-state ((buffer buffer))
  (when-let ((it (node-iterator buffer)))
    (state it)))

(defmethod make-node-iterator ((buffer buffer))
  "Make new node-iterator from BUFFER's parse-state."
  (make-node-iterator (parse-state buffer)))

(defmethod copy-iterator ((buffer buffer))
  (copy-iterator (node-iterator buffer)))

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

;; TODO add tests
(defmethod current-package ((buffer buffer))
  (let ((position (point buffer))
        (candidates (or
                     (in-package-nodes buffer)
                     (index-in-package-nodes buffer))))
    (when (and candidates (plusp (length candidates)))
      (find-if (lambda (node) (< (end node) position)) candidates :from-end t))))

(defmethod map-top-level-forms (function (buffer buffer))
  (map-top-level-forms function (parse-state buffer)))
