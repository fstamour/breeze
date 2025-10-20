(defpackage #:breeze.workspace
  (:documentation "")
  (:use #:cl #:breeze.buffer)
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
  (:import-from #:breeze.parser
                #:node-iterator
                #:copy-iterator
                #:make-node-iterator
                #:state
                #:source
                #:goto-position
                #:map-top-level-forms)
  (:import-from #:breeze.analysis
                #:node-string-designator
                #:with-match)
  (:import-from #:breeze.package
                #:map-top-level-in-package)
  (:export #:*workspace*
           #:make-workspace
           #:workspace
           #:add-to-workspace
           #:add-files-to-workspace
           #:find-buffer
           #:name
           #:filename
           #:parse-state
           #:point
           #:point-min
           #:point-max
           #:map-workpace-buffers
           #:locate-package-definition)
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


;;; workspace

;; TODO add mutex
(defclass workspace ()
  ((buffers
    :initform (make-hash-table :test 'equal)
    :accessor buffers
    :documentation ""))
  (:documentation ""))

(defun make-workspace ()
  (make-instance 'workspace))

(defvar *workspace* (make-instance 'workspace)
  "The workspace.")


;;; Buffers in the workspace

(defmethod find-buffer ((buffer-name string))
  (gethash buffer-name (buffers *workspace*)))

(defmethod ensure-buffer ((buffer-name string))
  (or (find-buffer buffer-name)
      (setf (gethash buffer-name (buffers *workspace*))
            (make-instance 'buffer :name buffer-name))))

(defmethod add-to-workspace ((context-plist cons))
  ;; TODO error or warn if name is not provided
  (when-let* ((name (getf context-plist :buffer-name))
              (buffer (ensure-buffer name)))
    (breeze.logging:log-debug "add-to-workspace buffer ~s" name)
    (when-let ((buffer-file-name (getf context-plist :buffer-file-name)))
      (setf (filename buffer) buffer-file-name))
    (when-let ((point (getf context-plist :point)))
      (setf (point buffer) point))
    (when-let ((point-min (getf context-plist :point-min)))
      (setf (point-min buffer) point-min))
    (when-let ((point-max (getf context-plist :point-max)))
      (setf (point-max buffer) point-max))
    (when-let ((new-content (getf context-plist :buffer-string)))
      (update-buffer-content buffer new-content))
    ;; return the buffer
    buffer))

  (defun add-files-to-workspace (files)
    (loop
      :for file :in (alexandria:ensure-list files)
      :for content = (alexandria:read-file-into-string file)
      :do (add-to-workspace `(:buffer-name ,(namestring file)
                              :buffer-string ,content
                              :point 0
                              :buffer-file-name ,file))))

(defun map-workpace-buffers (fn &optional (workspace *workspace*))
  (loop
    :for buffer-name :being :the :hash-key :of (buffers workspace)
      :using (hash-value buffer)
    :do (funcall fn buffer)))



;; TODO This might change per-project... Infer this
(defmethod in-package-cl-user-p ()
  "Whether to include (in-package #:cl-user) before a defpackage form or not."
  nil)

;; TODO move to utils
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

(defun infer-package-name-separator ()
  ;; TODO infer whether to use "." or "/" (or something else) as
  ;; "separator" in package names
  )

;; TODO replace .+ by + (e.g. breeze.+parachute => breeze+parachute
;; TODO project "abcd" file "abcd", package should be "abcd", not "abdc.abcd"
(defun infer-package-name-from-file (file-pathname)
  "Given a FILE-PATHNAME, infer a proper package name."
  (when file-pathname
    (let ((project (infer-project-name file-pathname))
          (test (when (infer-is-test-file file-pathname)
                  "test"))
          (name (pathname-name file-pathname)))
      ;; TODO not everyone would like to use "." as separator
      (format nil "~{~a~^.~}"
              (remove-if #'null (list project test name))))))

#+(or)
(trace
 infer-project-name
 infer-is-test-file
 infer-package-name-from-file)

;; TODO use breeze.analysis::map-top-level-forms to index defpackage
;; and in-package forms (and maybe others, like test definitions)

(defmethod find-test-directory ((namestring string)))


;;; Definitions-related utilities

;; TODO this code should be easily made into something (new
;; functions/methods) that locates _all_ package definitions, and
;; cache them into the *workspace*
(defmethod locate-package-definition ((package string))
  ;; TODO maybe cache the "match data" on the defpackage macros (that
  ;; would be hard to do correctly since the "match data" contains
  ;; iterator, and that there will be incremental parsing eventually).
  (map-workpace-buffers
   (lambda (buffer)
     (map-top-level-forms
      (lambda (node-iterator)
        (with-match (node-iterator
                     (:either
                      (cl:defpackage ?name)
                      ;; TODO matching uiop:define-package is
                      ;; currently broken/not implemented.
                      ((:symbol :define-package :uiop) ?name)))
          (when-let* ((package-name-node (get-bindings '?name))
                      (package-name (node-string-designator
                                     package-name-node)))
            (when (string-equal package package-name)
              (return-from locate-package-definition
                ;; TODO this should probably be yet another
                ;; object/class... "location" perhaps?
                (list :buffer buffer
                      :node-iterator node-iterator))))))
      buffer))))
