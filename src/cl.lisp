
;;;; See fndb.lisp and knownfn.lisp in <a href="https://github.com/sbcl/sbcl/blob/master/src/compiler/fndb.lisp">sbcl</a>
;;;; and <a href="https://github.com/phoe/portable-condition-system/blob/1307ec146d227a9d8ea42312c1ba2a5206a9eb3c/t/ansi-test-data.lisp">phoe/portable-condition-system</a>
;;;;
;;;;
;;;; See https://github.com/informatimago/lisp/blob/4bfb6893e7840b748648b749b22078f2facfee0a/common-lisp/lisp-reader/package-def.lisp
;;;; for a list of all CLs symbols

(cl:in-package #:common-lisp-user)

(defpackage #:breeze.cl
  (:documentation "Some useful metadata on cl's symbols")
  (:use :cl)
  (:export #:higher-order-function-p))

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

#++
(higher-order-function-p 'mapcar)
;; => 0

#++
(higher-order-function-p '+)
;; => nil
