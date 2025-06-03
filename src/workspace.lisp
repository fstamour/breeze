(defpackage #:breeze.workspace
  (:documentation "")
  (:use #:cl)
  (:import-from #:alexandria
                #:ends-with-subseq
                #:if-let
                #:when-let*
                #:when-let
                #:symbolicate
                #:lastcar)
  (:import-from #:breeze.indirection
                #:indirect)
  (:import-from #:breeze.utils
                #:find-version-control-root)
  ;; TODO circular dependency... should be fixable if we put "buffer"
  ;; either in its own package, or if it's moved into the reader's
  ;; package.
  (:import-from #:breeze.lossless-reader
                #:node-iterator
                #:copy-iterator
                #:make-node-iterator
                #:state
                #:source
                #:goto-position)
  (:export #:buffer
           #:*workspace*
           #:make-workspace
           #:workspace
           #:update-buffer-content
           #:add-to-workspace
           #:find-buffer
           #:name
           #:filename
           #:node-iterator
           #:parse-state
           #:point
           #:point-min
           #:point-max)
  (:export #:in-package-cl-user-p
           #:infer-package-name-from-file
           #:infer-project-name
           #:infer-is-test-file))

(in-package #:breeze.workspace)

#|

Goal(s):
- cache buffers' content between requests
- get a holistic view of project(s)
- make things more explicit (e.g. asd files, source files, test files, editors, etc)
- make cross-references and multi-file (or even multi-project) analysis and refactors easier/feasible
- keep track of which file and systems are loaded

Brainstorm:

Asd files and systems
Quicklisp project
File
FileSet
Version Control stuff

Design decision(s):

- There can be only 1 workspace, it has to handle _all_ the cases.
  - why? because otherwise you need some logic to _choose_ which
  workspace to use.

|#


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
    :documentation "If the buffer is narrowed, ... TODO"))
  (:documentation "Represents an editor's buffer (e.g. an opened file)

(Technically, it represents a buffer with the mode \"lisp-mode\"..."))

#++
(defun make-buffer (&key name string)
  (make-instance 'buffer :name name
                         :node-iterator (when string
                                          (make-node-iterator string))))

(defmethod parse-state ((buffer buffer))
  (when-let ((it (node-iterator buffer)))
    (state it)))

(defmethod make-node-iterator ((buffer buffer))
  "Make new node-iterator from BUFFER's parse-state."
  (make-node-iterator (parse-state buffer)))

(defmethod copy-iterator ((buffer buffer))
  (copy-iterator (node-iterator buffer)))


;;; workspace

;; TODO add mutex(es)?
(defclass workspace ()
  ((buffers
    :initform (make-hash-table :test 'equal)
    :accessor buffers
    :documentation ""))
  (:documentation ""))

(defun make-workspace ()
  (make-instance 'workspace))

(defmethod find-buffer ((buffer-name string))
  (gethash buffer-name (buffers *workspace*)))

(defmethod ensure-buffer ((buffer-name string))
  (or (find-buffer buffer-name)
      (setf (gethash buffer-name (buffers *workspace*))
            (make-instance 'buffer :name buffer-name))))

(defvar *workspace* (make-instance 'workspace)
  "The workspace.")

(defmethod update-buffer-content ((buffer buffer) new-content)
  "Update the workspace's buffer BUFFER-NAME's content"
  (when new-content
    (if-let ((old-node-iterator (node-iterator buffer)))
      (unless (string= (source (state old-node-iterator)) new-content)
        (breeze.logging:log-debug "re-parsing the buffer ~s from scratch" (name buffer))
        (setf (node-iterator buffer) (make-node-iterator new-content)))
      (progn (breeze.logging:log-debug "parsing the buffer ~s for the first time" (name buffer))
             (setf (node-iterator buffer) (make-node-iterator new-content))))))

;; (untrace add-to-workspace)
(defmethod add-to-workspace ((context-plist cons))
  ;; TODO error or warn if name is not provided
  (when-let* ((name (getf context-plist :buffer-name))
              (buffer (ensure-buffer name)))
    (breeze.logging:log-debug "add-to-workspace buffer ~s" name)
    (when-let ((buffer-file-name (getf context-plist :buffer-file-name)))
      (setf (filename buffer) buffer-file-name))
    ;; TODO remove the =(1- ...)= this is specific to emacs, it should
    ;; not be the responsibility of this package.
    (when-let ((point (getf context-plist :point)))
      (setf (point buffer) (1- point)))
    (when-let ((point-min (getf context-plist :point-min)))
      (setf (point-min buffer) (1- point-min)))
    (when-let ((point-max (getf context-plist :point-max)))
      (setf (point-max buffer) (1- point-max)))
    (when-let ((new-content (getf context-plist :buffer-string)))
      (update-buffer-content buffer new-content))
    ;; update the node-iterator's position
    (when-let ((node-iterator (node-iterator buffer))
               (point (point buffer)))
      (goto-position node-iterator point))
    ;; return the buffer
    buffer))



;; TODO This might change per-project... We could try to infer it?
(defmethod in-package-cl-user-p ()
  "Whether to include (in-package #:cl-user) before a defpackage form or not."
  nil)

(defun directory-name (pathname)
  (lastcar (pathname-directory pathname)))

;; TODO move infer-project-name here
(defun infer-project-name (path)
  "Try to infer the name of the project from the PATH."
  ;; Infer project-name by location .git folder
  (when path
    (if-let ((vc-root (indirect (find-version-control-root path))))
      (directory-name vc-root))))

;; TODO move infer-is-test-file here
(defun infer-is-test-file (path)
  "Try to infer if a file is part of the tests."
  (when path
    (member
     (directory-name path)
     '("test" "tests" "t")
     :test #'string-equal)))

;; TODO move infer-package-name-from-file here
(defun infer-package-name-from-file (file-pathname)
  "Given a FILE-PATHNAME, infer a proper package name."
  (when file-pathname
    (let ((project (infer-project-name file-pathname))
          (test (when (infer-is-test-file file-pathname)
                  "test"))
          (name (pathname-name file-pathname)))
      (format nil "~{~a~^.~}"
              (remove-if #'null (list project test name))))))

#+(or)
(trace
 infer-project-name
 infer-is-test-file
 infer-package-name-from-file)


(defmethod find-test-directory ((namestring string)))


#++
(defun check-in-package ()
  "Make sure the previous in-package form desginates a package that can
be found. If it's not the case (e.g. because the user forgot to define
a package and/or evaluate the form that defines the package) they show
a message and stop the current command."
  (let+ctx (nodes
            outer-node
            ;; Check if the closest defpackage was evaluated once
            (invalid-in-package
             (and nodes
                  outer-node
                  (validate-nearest-in-package nodes outer-node))))
    (when invalid-in-package
      (message "The nearest in-package form designates a package that doesn't exists: ~s"
               invalid-in-package)
      (return-from-command))))
