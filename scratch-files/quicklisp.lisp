(in-package #:breeze.quicklisp)

#++
(ql-dist:enabled-dists)
;; => (#<QL-DIST:DIST quicklisp 2023-02-15>)

(defparameter *ql* (ql-dist:find-dist "quicklisp"))

;; Download everything, without installing it.
(loop :for release :in (ql-dist:provided-releases *ql*)
      :do (ql-dist:ensure-local-archive-file release))

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
