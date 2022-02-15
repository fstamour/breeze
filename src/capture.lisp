(defpackage #:breeze.capture
  (:documentation "Utilities for quick capture and management of code.")
  (:use #:cl)
  (:export #:capture))

(in-package #:breeze.capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; The code below was copy-pasted from emacs lisp, it work work yet

#+ (or)
(defun breeze-list-lisp-files (directory)
  ;; just the name, no extension, no directory
  (loop for file in
                 (directory-files directory)
        when (and (not (string-prefix-p "." file))
                  (string-suffix-p ".lisp" file))
          collect (file-name-sans-extension file)))

#+ (or)
(define-skeleton breeze-insert-header-template
  "" ;; TODO docstring
  "" ;; empty prompt. ignored.
  \n
  "(ql:quickload '(alexandria))" \n
  \n
  ";; make it easier to debug" \n
  "(declaim (optimize (speed 0) (safety 3) (debug 3)))" \n
  \n
  "#|" \n
  \n
  "goal:" \n
  \n
  "motivation:" \n
  \n
  "first lead:" \n
  \n
  "|#")

#+ (or)
(defun capture ()
  ;; TODO docstring
  (interactive)
  ;; TODO check if directory exists, creates it if not.
  ;;   (mkdir breeze-capture-folder)
  (let* ((name (completing-read
                "name of the file and package: "
                (breeze-list-lisp-files breeze-capture-folder)))
         (file (concat breeze-capture-folder "/" name ".lisp"))
         (file-exists (file-exists-p file)))
    (find-file file)
    (unless file-exists
      (breeze-insert-defpackage))))
