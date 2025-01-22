(defpackage #:breeze.workspace
  (:documentation "")
  (:use #:cl)
  (:import-from #:alexandria
                #:ends-with-subseq
                #:if-let
                #:symbolicate
                #:lastcar)
  (:import-from #:breeze.indirection
                #:indirect)
  (:import-from #:breeze.utils
                #:find-version-control-root)
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



;; TODO this ain't used, ain't defined well either
(defvar *workspace* nil
  "The workspace.")

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
