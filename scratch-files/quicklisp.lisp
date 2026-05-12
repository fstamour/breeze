(in-package #:breeze.quicklisp)

#++
(ql-dist:enabled-dists)
;; => (#<QL-DIST:DIST quicklisp 2023-02-15>)
;; => (#<QL-DIST:DIST quicklisp 2026-01-01>)

(defparameter *ql* (ql-dist:find-dist "quicklisp"))

;; Download everything, without installing it.
(defparameter *local-archive-files*
  (loop :for release :in (ql-dist:provided-releases *ql*)
        :collect (ql-dist:ensure-local-archive-file release)))

#++
(/ (length (ql-dist:installed-releases *ql*))
   (length (ql-dist:provided-releases *ql*)))

;; Get the total size (in bytes) of all the archives
;; (require 'osicat)
(loop :for release :in (ql-dist:installed-releases *ql*)
      :sum (osicat-posix:stat-size
            (osicat-posix:stat
             (ql-dist:local-archive-file release))))
;; => 615869163

;; (float (/ 615869163 1024 1024))
;; => 587.33



(ql:quickload "tar")

(defparameter *alexandria*
  (ql-dist:local-archive-file (ql-dist:find-release-in-dist "alexandria" *ql*)))

(defun list-archive-entries (pathname)
  (tar:with-open-archive (archive pathname)
    (tar:do-entries (entry archive)
      ;; (format t "~&~a" (tar:name entry))
      )))

(ql:quickload "chipz")

(defun list-tgz (pathname)
  (let ((tar
          (with-open-file (stream pathname :direction :input
                                           :element-type '(unsigned-byte 8))
            (chipz:decompress nil 'chipz:gzip stream))))
    (flexi-streams:with-input-from-sequence (tar-stream tar)
      (list-archive-entries tar-stream))))

;; Found 1 bug in "archive": unhandled-read-header-error should "inherit" unhandled-error
;;
;; Now I'm getting errors because "archive" doesn't recogize entries
;; of type 50 (symlink) and 120 (pax (extended stuff)).
;;
;;

(dolist (pathname *local-archive-files*)
  (with-simple-restart (continue "Continue")
    (format t "~&~&~a" pathname)
    (list-tgz pathname)))

#|
Invalid tar header checksum 0 (wanted 6778)
#P"/home/fstamour/quicklisp/dists/quicklisp/archives/km-2-5-33.tgz"


Invalid stored block length
#P"/home/fstamour/quicklisp/dists/quicklisp/archives/simple-neural-network-20250622-git.tgz"
#P"/home/fstamour/quicklisp/dists/quicklisp/archives/snmp-6.1.tgz"

but tar and gunzip find no issues...
|#

(ql-dist:check-local-archive-file (ql-dist:find-release-in-dist "km" *ql*))
