(defpackage #:breeze.other-files
  (:documentation "Commands to jump to files relevant to the current context.")
  (:use #:cl #:breeze.command)
  (:export
   #:other-file
   #:other-file-other-window))

(in-package #:breeze.other-files)

(defun candidate-alt-dir (file)
  "Generate a list of alternate directories."
  (let ((root (or (breeze.utils:find-version-control-root file)
                  (some #'identity
                        (let ((asdf-list (breeze.utils:find-asdf-in-parent-directories file)))
                          (remove-duplicates
                           (mapcar #'uiop:pathname-directory-pathname asdf-list)
                           :test #'equal))))))
    (let* ((candidates (loop :for dir :in '("src/" "t/" "test/" "tests/")
                             :for pathname := (merge-pathnames dir root)
                             :collect pathname))
           (parentp (some (lambda (dir &aux (rel-path (enough-namestring file dir)))
                            (and (null (uiop:absolute-pathname-p rel-path))
                                 rel-path))
                          candidates))
           (parent (and parentp (uiop:pathname-directory-pathname parentp))))
      (if parent
          (append candidates
                  (mapcar (lambda (dir)
                            (merge-pathnames parent dir))
                          candidates))
          candidates))))

#++
(candidate-alt-dir
 (breeze.dogfood:breeze-relative-pathname
  "src/cmds/te/foo.lisp"))
#| =>
(#P"/home/fstamour/quicklisp/local-projects/breeze/src/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/t/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/test/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/tests/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/src/cmds/te/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/t/cmds/te/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/test/cmds/te/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/tests/cmds/te/")
|#

#++
(candidate-alt-dir
 (breeze.dogfood:breeze-relative-pathname
  "src/refactor.lisp"))
#|
=>
(#P"/home/fstamour/quicklisp/local-projects/breeze/src/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/t/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/test/"
 #P"/home/fstamour/quicklisp/local-projects/breeze/tests/")
|#

(defun pathname-filename (pathname)
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (format nil "~a~:[.~a~;~]" name (eq :unspecific type) type)))

(defun candidate-alt-files (file &aux (file (or (probe-file file) file)))
  (loop
    :with file-name := (pathname-filename file)
    :for altdir :in (candidate-alt-dir file)
    :for altfile := (merge-pathnames file-name altdir)
    :unless (equal file altfile)
    :collect altfile))

#++
(defun candidate-alt-files (file &aux (file (probe-file file)))
  (loop
    :with file-name := (pathname-filename file)
    :for altdir :in (candidate-alt-dir file)
    :for altfile := (merge-pathnames file-name altdir)
    :unless (equal file altfile)
    :collect altfile))

#++
(let* ((file (breeze.utils:breeze-relative-pathname "src/refactor.lisp")))
  (candidate-alt-files file))
;; => (#P"/home/fstamour/quicklisp/local-projects/breeze/src/" "refactor.lisp")

(defun %other-file (&optional other-window-p)
  (let* ((file (current-buffer-filename))
         (candidates (candidate-alt-files file))
         (candidates (or (remove-if-not #'probe-file candidates)
                         candidates)))
    (cond
      ((null candidates)
       (message "Couldn't find any \"other file\" for ~a" file))
      ((breeze.utils:length=1 candidates)
       (find-file (car candidates) other-window-p))
      (t
       (let ((choice (choose "Choose an other file: "
                             (mapcar #'namestring candidates))))
         (find-file choice other-window-p))))))

(define-command other-file ()
  "Find the alternative file for the current file."
  (%other-file))

(define-command other-file-other-window ()
  "Find the alternative file for the current file in the other window."
  (%other-file t))

#++
(when path
    (if-let ((vc-root (indirect (find-version-control-root path))))
      (mapcar)))

#++
(let ((vc-root (find-version-control-root (breeze.utils:breeze-relative-pathname "."))))
  (mapcar (lambda (directory)
            (merge-pathnames directory vc-root))
          '("" "src/" "source/" "sources/")))

;; 1. generate dirs from "vc-root" '("src" "t" "test" "tests")
;; 2. find in which directory is the current file
;; 3. find which alternative directory exists
;; 4. find which alternative file exists

;; On second thought, the "find test directory"/"find test files" should be part of the "workspace".

#++
(if-let (buffer-file-name))
#++
(if-let ((vc-root (indirect (find-version-control-root path))))
  (directory-name vc-root))
