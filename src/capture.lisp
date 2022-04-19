(defpackage #:breeze.capture
  (:documentation "Utilities for quick capture and management of code.")
  (:use #:cl)
  (:import-from #:breeze.utils
                #:subseq-displaced
                #:whitespacep)
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

(defun leading-whitespaces (string)
  (with-input-from-string (input string)
    ;; Skip the first line
    (when (read-line input nil nil)
      (loop :for line = (read-line input nil nil)
            :while line
            :for leading-whitespaces = (position-if-not #'whitespacep line)
            :when leading-whitespaces
              :minimize leading-whitespaces))))

(defun remove-indentation (string)
  (let ((indentation (leading-whitespaces string)))
    (with-input-from-string (input *capture-template*)
      (with-output-to-string (output)
        (loop :for line = (read-line input nil nil)
              :while line
              :for leading-whitespaces = (position-if-not #'whitespacep line)
              :if (and leading-whitespaces
                       (>= leading-whitespaces indentation))
                :do (write-string (subseq-displaced line indentation) output)
              :else
                :do (write-string line output)
              :do (write-char #\newline output))))))

(defun populate (pathname)
  (with-open-file (output pathname
                          :if-exists :error
                          :if-does-not-exist :create
                          :direction :output)
    (format output (remove-indentation *capture-template*))))

(define-command capture ()
  "Quickly create a lisp file in a pre-determined directory."
  ;; TODO check if *capture-folder* exists
  ;; We use "chose" instead of "read-string" so that the user can
  ;; easily see if he's trying to create a file with a name that
  ;; already exists.
  (let* ((name (concatenate 'string
                            (choose "Name of the file and package: "
                                    (list-existing-captured-files))
                            ".lisp"))
         (pathname (merge-pathnames name *capture-folder*)))
    (unless (probe-file pathname)
      (populate pathname))
    (find-file pathname)))
