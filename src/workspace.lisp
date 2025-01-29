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
  (:import-from #:breeze.lossless-reader
                #:node-iterator
                #:make-node-iterator
                #:state
                #:source
                #:goto-position)
  (:export #:add-to-workspace
           #:find-buffer
           #:name
           #:filename
           #:parse-tree
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
    :accessor parse-tree
    :documentation "Node at point")
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

;; TODO add mutex(es)?
(defclass workspace ()
  ((buffers
    :initform (make-hash-table :test 'equal)
    :accessor buffers
    :documentation ""))
  (:documentation ""))

(defmethod find-buffer ((buffer-name string))
  (gethash buffer-name (buffers *workspace*)))

(defmethod ensure-buffer ((buffer-name string))
  (or (find-buffer buffer-name)
      (setf (gethash buffer-name (buffers *workspace*))
            (make-instance 'buffer :name buffer-name))))

(defvar *workspace* (make-instance 'workspace)
  "The workspace.")

;; (untrace add-to-workspace)
(defmethod add-to-workspace ((context-plist cons))
  ;; TODO check that the context contains enough stuff to make a
  ;; buffer object that is not completly useless.
  ;; TODO check if the buffer is not already in the workspace
  (breeze.logging:log-info "Adding the file ~s to the workspace"
                           (getf context-plist :buffer-file-name))
  (when-let* ((name (getf context-plist :buffer-name))
              (buffer (ensure-buffer name)))
    (setf (filename buffer) (getf context-plist :buffer-file-name)
          (point buffer) (getf context-plist :point)
          (point-min buffer) (getf context-plist :point-min)
          (point-max buffer) (getf context-plist :point-max))
    ;; Update the content of the buffer
    (when-let ((new-content (getf context-plist :buffer-string)))
      (if-let ((old-node-iterator (parse-tree buffer)))
        (unless (string= (source (state old-node-iterator)) new-content)
          (breeze.logging:log-debug "re-parsing the buffer ~s from scratch" name)
          (setf (parse-tree buffer) (make-node-iterator new-content)))
        (progn (breeze.logging:log-debug "parsing the buffer ~s for the first time" name)
               (setf (parse-tree buffer) (make-node-iterator new-content)))))
    ;; Update the position (point)
    (when-let ((point (getf context-plist :point)))
      (unless (= point (point buffer))
        (setf (point buffer) point)
        (goto-position (parse-tree buffer) point)))
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

#+ (or)
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
