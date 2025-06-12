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
  (:import-from #:breeze.lossless-reader
                #:node-iterator
                #:copy-iterator
                #:make-node-iterator
                #:state
                #:source
                #:goto-position
                #:map-top-level-forms)
  (:import-from #:breeze.analysis
                #:node-string-designator-string
                #:with-match)
  (:import-from #:breeze.package
                #:map-top-level-in-package)
  (:export #:*workspace*
           #:make-workspace
           #:workspace
           #:add-to-workspace
           #:find-buffer
           #:name
           #:filename
           #:parse-state
           #:point
           #:point-min
           #:point-max)
  (:export #:in-package-cl-user-p
           #:infer-package-name-from-file
           #:infer-project-name
           #:infer-is-test-file)
  (:export #:after-change-function))

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
    ;; update the node-iterator's position
    (when-let ((node-iterator (node-iterator buffer))
               (point (point buffer)))
      (goto-position node-iterator point))
    ;; return the buffer
    buffer))

;; TODO
;; (defun add-systemS-to-workspace ...)



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

;; TODO move infer-package-name-from-file here
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



;;; Incremental parsing (the interface with the editor at least)

(defun push-edit (edit)
  (declare (ignore edit))
  #++ (print edit))

;; TODO keep track of the buffers/files, process these kind of edits
;; "object":
;;
;; (:DELETE-AT 18361 1)
;; (:INSERT-AT 17591 ";")

(defun after-change-function (start stop length &rest rest
                                              &key
                                                buffer-name
                                                buffer-file-name
                                                insertion
                                              &allow-other-keys)
  (declare (ignorable start stop length rest buffer-file-name insertion)) ; yea, you heard me
  ;; TODO the following form is just a hack to keep the breeze's
  ;; buffers in sync with the editor's buffers
  (when-let ((buffer (find-buffer buffer-name)))
    (setf (node-iterator buffer) nil))
  ;; consider ignore-error + logs, because if something goes wrong in
  ;; this function, editing is going to be funked.
  (push-edit
   (cond
     ((zerop length)
      (list :insert-at start insertion))
     ((plusp length)
      (list :delete-at start length))
     (t :unknown-edit))))

;; TODO add NOTE: "can't splice comment", but I wish I could
;; e.g.  `  ;; (some | code)`
;; paredit-splice-sexp or paredit-splice-sexp-killing-backward



;;; Definitions-related utilities

;; TODO add tests
(defmethod locate-package-definition ((package string) #| TODO haystack |#)
  ;; TODO don't look only in the current buffer
  ;; TODO maybe cache the "match data" on the defpackage macros
  (loop :for buffer :in (buffers *workspace*)
        :do (map-top-level-forms
             (lambda (node-iterator)
               (with-match (node-iterator
                            (:alternation
                             (cl:defpackage ?name)
                             (uiop:define-package ?name)))
                 (when-let* ((package-name-node (get-bindings '?name))
                             (package-name (node-string-designator-string
                                            package-name-node)))
                   (when (string= package package-name)
                     (return-from locate-package-definition node-iterator)))))
             buffer)))
