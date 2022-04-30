(cl:in-package #:common-lisp-user)

(defpackage #:breeze.test.refactor
  (:use :cl #:breeze.refactor)
  (:import-from #:breeze.command
                #:cancel-command
                #:continue-command)
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
                #:function-node-p)
  (:import-from #:parachute
                #:define-test
                #:is))

(in-package #:breeze.test.refactor)

(defparameter *directory* "./")

(defun drive-command (fn context-plist inputs)
  (cancel-command)
  (loop :for request = (apply fn context-plist)
          :then (continue-command)
        :collect request
        :while (and request
                    (not (string= "done" (car request))))))

(define-test insert-handler-bind-form
  (is equal
      '(("insert" "(handler-bind
  ((error #'(lambda (condition)
    (describe condition *debug-io*))))
  (frobnicate))")
        ("done"))
      (drive-command #'insert-handler-bind-form
                     '()
                     nil)))

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
