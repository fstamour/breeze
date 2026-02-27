(defpackage #:breeze.cli
  (:documentation "Command line interface")
  (:use #:cl)
  (:export #:main))

(in-package #:breeze.cli)

(defun main ()
  (print "I BEG YOU! DONT USE THE CLI, ITS NOT READY!")
  ;; list all *.lisp file and print any "diagnostics"
  (let* ((root *default-pathname-defaults*)
         (wild-dir `(,@(pathname-directory root) :wild-inferiors)))
    (breeze.workspace:add-files-to-workspace
     (directory
      (make-pathname :directory wild-dir :name :wild :type "asd")))
    (breeze.workspace:add-files-to-workspace
     (directory
      (make-pathname :directory wild-dir :name :wild :type "lisp")))
    (breeze.workspace:map-workpace-buffers
     (lambda (buffer)
       (format t "~&~a"
               (enough-namestring (breeze.buffer:filename buffer) root))
       (loop
         :for diagnostic :in (breeze.lint:lint-buffer buffer
                                                      :livep nil)
         :do (format t "~&~a" diagnostic))))))
