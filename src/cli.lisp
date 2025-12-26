(defpackage #:breeze.cli
  (:documentation "Command line interface")
  (:use #:cl)
  (:export #:main))

(in-package #:breeze.cli)

(defun main ()
  ;; list all *.lisp file and print any "diagnostics"
  (breeze.workspace:add-files-to-workspace
   (directory
    (make-pathname
     :directory
     `(,@(pathname-directory
          *default-pathname-defaults*)
         :wild-inferiors)
     :name :wild
     :type "lisp")))
  (breeze.workspace:map-workpace-buffers
   (lambda (buffer)
     (format t "~&~a" (breeze.buffer:filename buffer))
     (loop
       :for diagnostic :in (breeze.lint:lint-buffer buffer
                                                    :livep nil)
       :do (format t "~&~a" diagnostic)))))
