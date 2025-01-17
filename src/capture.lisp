(defpackage #:breeze.capture
  (:documentation "Utilities for quick capture and management of code.")
  (:use #:cl)
  (:import-from #:breeze.string
                #:remove-indentation
                #:whitespacep)
  (:import-from #:breeze.utils
                #:subseq-displaced)
  (:import-from #:breeze.command
                #:choose
                #:define-command
                #:message
                #:find-file)
  (:import-from #:breeze.config
                #:*capture-folder*
                #:*capture-template*)
  (:export #:capture))

(in-package #:breeze.capture)

(defun list-existing-captured-files ()
  (mapcar #'pathname-name
          (directory
           (merge-pathnames "*.lisp" *capture-folder*))))

(defun populate (pathname)
  (with-open-file (output pathname
                          :if-exists :error
                          :if-does-not-exist :create
                          :direction :output)
    (format output (remove-indentation *capture-template*))))

(define-command capture ()
  "Quickly create a lisp file in a pre-determined directory."
  ;; TODO Make sure *capture-folder* is set
  ;; TODO Otherwise, ask the user to choose a directory _and save it_
  (unless *capture-folder*
    (error "*capture-folder* not set"))
  ;; TODO ensure that *capture-folder* is a valid directory pathname
  ;; TODO check if *capture-folder* exists
  ;; We use "chose" instead of "read-string" so that the user can
  ;; easily see if he's trying to create a file with a name that
  ;; already exists.
  (let* ((files (list-existing-captured-files))
         (name (concatenate 'string
                            (choose "Name of the file and package: " files)
                            ".lisp"))
         (pathname (merge-pathnames name *capture-folder*)))
    (unless (probe-file pathname)
      (populate pathname))
    (find-file pathname)))
