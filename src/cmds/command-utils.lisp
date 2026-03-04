(defpackage #:breeze.command-utils
  (:documentation "Utilities to write commands")
  (:use #:cl #:breeze.command #:breeze.analysis)
  (:import-from #:breeze.string
                #:trim-whitespace
                #:ensure-circumfix
                #:ensure-circumfixes)
  (:export #:create-directory-or-abort
           #:pulse-node
           #:current-node
           #:normalize-docstring
           #:normalize-lambda-list
           #:ensure-space))

(in-package #:breeze.command-utils)

(defun create-directory-or-abort (directory)
  (unless (probe-file directory)
    (cond
      ((ask-y-or-n-p "The directory ~s doesn't exist, do you want to create it?")
       (ensure-directories-exist directory))
      (t (message "Aborted by user")
         (return-from-command)))))

(defun node-at-point ())

;; TODO This should go in a file for "commands that uses parse trees"
(defun pulse-node (node)
  (pulse (start node) (end node)))

(defun current-node (&key pulsep)
  (alexandria:when-let*
      ((buffer (current-buffer))
       (node-iterator (node-iterator buffer)))
    (when pulsep (pulse-node node-iterator))
    node-iterator))

;; TODO Maybe make a command "replace-form" or "replace-car"

#++ ;; TODO this is work in progress
(defun ensure-in-package (package-name)
  "Insert an ~cl:in-package~ form, if necessary."
  (let (($package (current-package))))
  (insert "(cl:in-package #:asdf-user)~%~%"))


;; TODO method "ask licence" and "ask author"
;; (methods, not defun, for better testability and customization)
;; TODO choose-package


;; TODO more tests (nil, "", "\"\"", ...)
(defun normalize-docstring (string)
  "Try to normalize docstring entered by the user into something that can
be inserted correctly into a buffer.
Returns nil if string is nil, empty, contains only whitespaces, or
contains only two or less double-quote."
  (when string
    (let ((trimmed (trim-whitespace string)))
      (when (plusp (length trimmed))
        (let ((quoted (ensure-circumfix "\"" trimmed)))
          (when (< 2 (length quoted))
            quoted))))))

(defun normalize-lambda-list (string)
  "Try to normalize lambda-list entered by the user into something that can
be inserted correctly into a buffer."
  (ensure-circumfix "(" (trim-whitespace string) ")"))

(defun ensure-space ()
  "Makes sure there is a space preceding the point."
  (let ((node (previous-sibling (current-node))))
    (when (and node
               (not (whitespace-node-p node)))
      (insert " "))))
