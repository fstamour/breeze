(defpackage #:breeze.project
  (:documentation "Project scaffolding utilities")
  (:use #:cl)
  (:import-from #:breeze.utils
                #:length>1?)
  (:import-from #:breeze.command
                #:read-string
                #:ask-y-or-n-p
                #:return-from-command
                #:choose)
  (:export #:project
           #:confirm-scaffold-directory
           #:choose-local-project-directories))

(in-package #:breeze.project)

(defclass project ()
  ((name
    :initform nil
    :initarg :name
    :accessor name
    :documentation "Name of the poject,")
   (root-dir
    :initform nil
    :initarg :root-dir
    :accessor root-dir
    :documentation "The path of the root directory of the project.")))

;; &key depends-on author include-copyright license
;; name template-directory template-parameters
;; &allow-other-keys

(defun ql-local-project-directories ()
  "Get the list of quicklisp local-projects directories (as strings)."
  #+quicklisp
  (mapcar #'namestring
          ql:*local-project-directories*)
  ;; To appease sbcl's type checking xD
  #-quicklisp (list))

(defun choose-local-project-directories ()
  (let ((directories (ql-local-project-directories)))
    (cond
      ((null directories)
       (read-string
        "Please enter the directory where to create the project: "))
      ((length>1? directories)
       (choose "Please choose where to create the project: "
               directories))
      (t
       (first directories)))))

(defun confirm-scaffold-directory (directory)
  "If a user choose to scaffold a project into a directory that already
exists, we need their confirmation to continue."
  (when (probe-file directory)
    (unless (ask-y-or-n-p
             "The directory \"~A\" already exists. Scaffolding might result in data loss, are you sure you want to continue? (y/n) "
             directory)
      (return-from-command)))
  directory)
