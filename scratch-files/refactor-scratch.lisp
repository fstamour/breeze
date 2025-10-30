;;; Trying to refactor stuff

(cl:defpackage #:refactor-scratch
  (:use :cl)
  (:import-from #:breeze.parser
                #:parse))

(cl:in-package #:refactor-scratch)

(defun test-refactor-if (string)
  (let ((node (first (parse-string string))))
    (when (and (if-p node)
               (= 3 (node-length node)))
      (setf (node-content (first (node-content node)))
            (if (null-node-p (node-lastcar node))
                "unless"
                "when"))
      (unparse-to-string (list node)))))

(test-refactor-if "(if x nil)")
"(unless x nil)"

(test-refactor-if "(if x 32)")
"(when x 32)"

(test-refactor-if
 "(if #| comment |# 'x NIL)")
"(unless #| comment |# 'x nil)"

(test-refactor-if
 "(IF
    'x NIL)")
"(unless
    'x nil)" ;; FIXME it should be "NIL"



;; Forms to add to defsystem to test with parachute
(let ((system-name "breeze"))
  (let ((test-system (format nil "~a/test" system-name))
        (test-package (format nil "~a/test" system-name))
        (test-function system-name))
    (format nil
            "~{~A~}"
            (list
             ":in-order-to ((test-op (load-op #:" test-system")))
 :perform
   (test-op (o c)
   (symbol-call
    '#:parachute '#:test
    (find-symbol (symbol-name '#:" test-function ")
                 (find-package '#:" test-package "))
    :report (find-symbol \"INTERACTIVE\" \"PARACHUTE\")))"))))
