(defpackage #:breeze.package-commands
  (:documentation "Package-related commands")
  (:use #:cl
        #:breeze.analysis
        #:breeze.command
        #:breeze.package
        #:breeze.workspace)
  (:export #:insert-defpackage
           #:insert-local-nicknames
           #:insert-in-package-cl-user))

(in-package #:breeze.package-commands)


;;; Commands for inserting and modifying package-related forms

(define-command insert-defpackage ()
  "Insert a defpackage form."
  (declare (context :top-level))
  (let ((package-name
          (read-string
           "Name of the package: "
           (infer-package-name-from-file (current-buffer-filename)))))
    (when (in-package-cl-user-p)
      (insert
       "(cl:in-package #:cl-user)~%~%"))
    (if nil ; TODO
        (insert "(uiop:define-package ")
        (insert "(defpackage "))
    ;; TODO don't insert the (in-package ...) if it already exists
    (insert
     "#:~a~
    ~%  (:documentation \"\")~
    ~%  (:use #:cl))~
    ~%~
    ~%(in-package #:~a)"
     package-name package-name)))

(define-command insert-local-nicknames ()
  "Insert local nicknames."
  (declare (context (:child-of :package-definition))) ; TODO
  (insert
   "(:local-nicknames ~{~a~^~%~})"
   (loop :for name = (read-string "Name of the package to alias: ")
         :while (plusp (length name))
         :for alias = (read-string "Alias of the package: ")
         :collect (format nil "(#:~a #:~a)" alias name))))

(define-command insert-in-package-cl-user ()
  "Insert (cl:in-package #:cl-user)"
  (declare (context :top-level))
  (insert "(cl:in-package #:cl-user)"))
