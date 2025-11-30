(defpackage #:breeze.quicklisp
  (:documentation "Utilities for quicklisp")
  (:use #:cl #:breeze.command)
  (:export #:quickload))

(in-package #:breeze.quicklisp)

#+quicklisp
(define-command quickload ()
  "Choose a system to load with quickload."
  (let* ((systems (ql-dist:provided-systems t))
         (mapping (make-hash-table :test 'equal))
         (choices (mapcar (lambda (system)
                            (let* ((release (ql-dist:release system))
                                   (string (format nil "~a (~a ~a)"
                                                   (ql-dist:name system)
                                                   (ql-dist:prefix release)
                                                   (ql-dist:short-description (ql-dist:dist release)))))
                              (setf (gethash string mapping) system)
                              string))
                          systems))
         (choice (choose "Choose a system: " choices))
         (chosen-system (gethash choice mapping)))
    (cond
      (chosen-system #| TODO |#)
      (t #| TODO |#))))


;; TODO create a similar commands that uses asdf only
;; TODO create a similar command that calls (asdf:test-system)
;; TODO should extract the "choose a system" part
;;
;; other "quicklisp" commands:
;; - intstall ?? meh
;; - update client
;; - update dist
;; - check for update?
