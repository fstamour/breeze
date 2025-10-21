(cl:in-package #:cl-user)

(defpackage #:breeze.test.main
  (:documentation "Entry point for all tests in breeze.")
  (:use #:cl)
  (:export #:run-breeze-tests)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:isnt
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.main)

(defparameter cl-user::*exit-on-test-failures* nil)

;; (setf parachute:*silence-plain-compilation-errors-p* nil)

(defun list-source-files-under (system dirname)
  (directory
   (make-pathname
    :directory (pathname-directory
                (asdf:system-relative-pathname system dirname))
    :name :wild
    :type "lisp")))

#++
(define-test+run "all source files should be included in the system definition"
  (finish
   (let* ((system 'breeze)
          (dirnames '("src/" "tests/"))
          (known-files
            (remove-if (lambda (pathname) (string= (pathname-type pathname) "asd"))
                       (breeze.asdf:find-all-related-files 'breeze)))
          (existing-files (mapcan (lambda (dirname) (list-source-files-under system dirname))
                                  dirnames)))
     (is = 0 (- (length known-files) (length existing-files))
         "There should be the same number of files referenced in ~s's system definition file that is under the directories ~{~s~^ ~}.
~d files are referenced in the system definition file.
~d *.lisp files where actually found."
         system dirnames
         (length known-files) (length existing-files))
     (loop :for unknown-file :in (mapcar (lambda (pathname)
                                           (enough-namestring pathname
                                                              (asdf:system-relative-pathname system "")))
                                         (set-difference existing-files known-files))
           :do (false unknown-file "The file ~s is not part of ~s's system definition file."
                      unknown-file system)))))


#++
(define-test+run "all source files should have a corresponding test file"
  (finish
   (let* ((system (asdf:find-system 'breeze))
          (files (mapcar (lambda (pathname)
                           (breeze.asdf:system-enough-pathname pathname system))
                         (breeze.asdf:find-all-related-files system)))
          (top-level-files (remove-if #'pathname-directory files))
          (directories (remove-duplicates (mapcar #'pathname-directory files)))
          (directories-by-file (loop
                                 :with directories-by-file = (make-hash-table :test 'equal)
                                 :for file :in files
                                 :for name := (format nil "~a.~a" (pathname-name file) (pathname-type file))
                                 :for directory := (pathname-directory file)
                                 :when directory
                                   :do (push (second directory) (gethash name directories-by-file))
                                 :finally (return directories-by-file))))
     (is equal '(#P"breeze.asd") top-level-files)
     (is equal '((:relative "tests") () (:relative "src")) directories)
     (loop :for file :being :the :hash-key :of directories-by-file
             :using (hash-value directories)
           :when (< (length directories) 2)
             :do (isnt string= "src" (first directories)
                       "The file \"src/~a\" doesn't have a correcsponding test file under \"tests/\"." file)))))


(defun run-breeze-tests (&key exitp (report 'parachute:largescale))
  "Run breeze's tests."
  (let ((packages (breeze.xref:find-packages-by-prefix "breeze.test")))
    (format *trace-output*
            "~&About to run tests for the packages:~%~{  - ~A~%~}"
            packages)
    (finish-output *trace-output*)
    (let ((cl-user::*exit-on-test-failures* exitp))
      (parachute:test packages :report report))))
