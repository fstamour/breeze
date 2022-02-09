;;;; See fndb.lisp and knownfn.lisp in sbcl

(cl:in-package #:common-lisp-user)

(defpackage #:breeze.cl
  (:documentation "Some useful metadata on cl's symbols")
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:breeze.cl)


(defun higher-order-function-p (function)
  (multiple-value-bind (position _)
      (gethash function
	       #.(alexandria:alist-hash-table '((funcall . 0) (map-into . 1) (mapcon . 0) (mapl . 0) (set-pprint-dispatch . 1)
						(mapcar . 0) (reduce . 0) (maplist . 0) (complement . 0)
						(shared-initialize . 17) (map . 1) (mapcan . 0) (set-macro-character . 1)
						(mapc . 0) (set-dispatch-macro-character . 2) (apply . 0))))
    (declare (ignore _))
    position))

#+ (or)
(higher-order-function-p 'mapcar)
;; => 0
#+ (or)
(higher-order-function-p '+)
;; => nil
