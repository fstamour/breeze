
(defpackage #:breeze.file-watcher
  (:use :cl #:alexandria)
  (:export )
  (:import-from #:uiop
                #:while-collecting
                #:collect-sub*directories
                #:directory-files))

(in-package #:breeze.file-watcher)

(let ((system-designator 'breeze))
  (asdf/system:system-source-file
   (asdf/system:find-system system-designator)))

(basdf:system-files 'breeze)

;; from reddit
(defun watch-pathnames (pathnames callback &key (sleep 1))
  "In a loop, look at pathnames and call callback if their modtime changed or if they don't exists anymore."
  (unless (and (typep pathnames 'sequence)
               (not (stringp pathnames)))
    (setf pathnames (list pathnames)))
  (flet ((modtime (pathname)
           (when (probe-file pathname)
             (file-write-date pathname))))
    (let ((modtimes (make-hash-table :test 'equal)))
      (map nil (lambda (pathname)
                 (setf (gethash pathname modtimes) (modtime pathname)))
           pathnames)
      (catch 'done-watching
        (loop
          #+ nil (format t "~%WATCHING~%")
          #+ nil (force-output)
          (sleep sleep)
          (map nil (lambda (pathname)
                     (let ((modtime (modtime pathname)))
                       #+ nil (format t "~&modtime for ~a: ~a~%"
                                      pathname modtime)
                       (unless (eql modtime
                                    (gethash pathname modtimes))
                         (funcall callback pathname)
                         (setf (gethash pathname modtimes) modtime))))
               pathnames))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; from looset
(defvar *default-skipped-directories*
  '(".idea" ;; Idea's IDEs
    ".git"
    "pacakges") ;; .Net package
  "List of well-known directories that we usually want to skip")

(defun skip-directories (pathname test directories)
  "Takes a pathname, returns nil if the pathname's directory matches one of the directories."
  (if directories
      (not
       (member
        (lastcar (pathname-directory pathname))
        directories
        :test test))
    t))

(defun pathname-type-member (pathname types test)
  (member (pathname-type pathname) types :test test))

;; this is different from the version from looset
(defun find-files (root-directory
                   &key (test
                         #+windows #'string-equal
                         #-windows #'string=)
                     (skip-directories *default-skipped-directories*)
                     include-file-types
                     file-predicate)
  "Returns a list of files that matches the critera."
  (while-collecting
      (collect-file)
    (collect-sub*directories
     ;; directory
     root-directory
     ;; collectp
     t
     ;; recursep
     (lambda (pathname)
       (skip-directories pathname test skip-directories))
     ;; collector
     #'(lambda (directory)
         (dolist (file (directory-files directory))
           (when (and (pathname-type-member file include-file-types test)
                      (or (not file-predicate) (funcall file-predicate file)))
             (collect-file file)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; from looset


#|

;; kill worker threads
(mapcar 'bt:destroy-thread
  (remove-if-not
     #'(lambda (thread) (string= "worker" (bt:thread-name thread)))
    (bt:all-threads)))

(find-files "~/quicklisp/local-projects/breeze/"
  :include-file-types '("lisp"))



(watch-pathnames (find-files "~/quicklisp/local-projects/breeze/"
                             :include-file-types '("lisp"))
                 (lambda (pathname)
                   (print pathname)
                   (force-output)
                   (throw 'done-watching nil)))

(defun watch-system (system-designator)
  "Watch every lisp files in the system"
  (watch-pathnames
    (breeze.asdf:system-files '#:breeze))
   ;; TODO Rendu here
)

|#

(defclass file-watcher)


