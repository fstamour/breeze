(defpackage #:breeze.other-files
  (:documentation "Commands to jump to files relevant to the current context.")
  (:use #:cl #:breeze.command)
  (:export
   #:other-file
   #:other-file-other-window))

(in-package #:breeze.other-files)

(defun candidate-alt-dir (file)
  "Generate a list of alternate directories."
  ;; start by trying to find the root of the project.
  (let ((root (or
               ;; prefer the VCS root (git, hg, etc.)
               (breeze.utils:find-version-control-root file)
               ;; fallback: use the directory of the nearest .asd
               ;; file. ~(some #'identity ...)~ picks the first
               ;; non-nil element.
               (some #'identity
                     (let ((asdf-list (breeze.utils:find-asdf-in-parent-directories file)))
                       (remove-duplicates
                        (mapcar #'uiop:pathname-directory-pathname asdf-list)
                        :test #'equal))))))
    (let* (;; generate a bunch of "candidate directories"
           (candidates (loop :for dir :in '("src/" "t/" "test/" "tests/")
                             :for pathname := (merge-pathnames dir root)
                             :collect pathname))
           ;; find if (and which) one of the "candidate directory"
           ;; contains FILE, and compute the relative sub-path within
           ;; it (e.g. is the file "src/cmds/te/foo.lisp" under the
           ;; candidate directory "t/" or
           ;; "src/"?). `enough-namestring' returns an absolute path
           ;; when the file is NOT under that base, so we check if it
           ;; returns an absolute path or not.
           (parentp (some (lambda (dir &aux (rel-path (enough-namestring file dir)))
                            (and (null (uiop:absolute-pathname-p rel-path))
                                 rel-path))
                          candidates))
           ;; if FILE is actually under one of the candidate
           ;; directory, strip the filename from the relative-path to
           ;; get just the subdirectory (e.g. "cmds/te/")
           (parent (and parentp (uiop:pathname-directory-pathname parentp))))
      ;; If the file is in a subdirectory of a candidate dir, also add
      ;; the subdirectory path under each of the other candidate dirs.
      ;; e.g. if file is "src/cmds/te/foo.lisp", also include
      ;; "tests/cmds/te/", "t/cmds/te/", etc
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
  "Return the filename including the extension (if any)."
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (format nil "~a~:[.~a~;~]"
            name
            (or (null type) (eq :unspecific type))
            type)))

;; Here probe-file is just used to get a namestring (instead of a
;; pathname)??
(defun candidate-alt-files (file &aux (file (or (probe-file file) file)))
  "Return a list of candidate alternate files for FILE (e.g. return the
test file for a source file)."
  (loop
    :with file-name := (pathname-filename file)
    :for altdir :in (candidate-alt-dir file)
    :for altfile := (merge-pathnames file-name altdir)
    :unless (equal file altfile)
    :collect altfile))


#++
(let* ((file (breeze.dogfood:breeze-relative-pathname "src/refactor.lisp")))
  (candidate-alt-files file))
;; => (#P"/home/fstamour/quicklisp/local-projects/breeze/src/" "refactor.lisp")

(defun %other-file (&optional other-window-p)
  "Jump to an alternate file for the current buffer (e.g. source ↔ test).
If OTHER-WINDOW-P is non-nil, open the file in another window."
  (let* ((file (current-buffer-filename))
         ;; equalp for case-insensitivity
         (candidates (remove-duplicates
                      (candidate-alt-files file)
                      :test #'equalp))
         ;; prefer candidates that already exist on disk; fall back to
         ;; all candidates if none exist (so the user can still jump
         ;; to a new file)
         (candidates (or (remove-if-not
                          (lambda (c) (probe-file c))
                          candidates)
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
  "Open the alternative file for the current file."
  (%other-file))

(define-command other-file-other-window ()
  "Open the alternative file for the current file, in another window."
  (%other-file t))

#++
(when path
    (if-let ((vc-root (find-version-control-root path)))
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
(if-let ((vc-root (find-version-control-root path)))
  (directory-name vc-root))


;; TODO if the file doesn't exist, but one of the "t" "test" "tests"
;; exits, only suggest that one


;; TODO make the command other-file work when there's only one file
;; for the tests
