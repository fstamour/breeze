(cl:in-package #:common-lisp-user)

(defpackage #:breeze.refactor.test
  (:use :cl #:breeze.refactor)
  (:import-from #:breeze.test
                #:deftest
                #:is)
  (:import-from #:breeze.reader
                #:node-content
                #:parse-string
                #:unparse-to-string

                ;; Types of node
                #:skipped-node
                #:symbol-node
                #:read-eval-node
                #:character-node
                #:list-node
                #:function-node

                ;; Type predicates
                #:skipped-node-p
                #:symbol-node-p
                #:read-eval-node-p
                #:character-node-p
                #:list-node-p
                #:function-node-p))

(in-package #:breeze.refactor.test)


(defparameter *directory* "./")

(defun test-quickfix (buffer-name pre post)
  (let ((buffer-file-name (merge-pathnames buffer-name *directory*))
        (point (length pre))
        (buffer-string (concatenate 'string pre post)))
    (quickfix
     :buffer-name buffer-name
     :buffer-file-name buffer-file-name
     :buffer-string buffer-string
     :point point
     :point-min 0
     :point-max (length buffer-string))))

#+ (or)
(test-quickfix
 "mapcar.lisp"
 "(mapcar " ")")

#+ (or)
(context-buffer-string
 (alexandria:plist-hash-table
  '(:buffer-string "asdf")))
