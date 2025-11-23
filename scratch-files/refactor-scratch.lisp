;;; Trying to refactor stuff

(cl:defpackage #:refactor-scratch
  (:use :cl #:breeze.parser #:breeze.workspace #:breeze.analysis))

(cl:in-package #:refactor-scratch)

;; Trying to update a (parachute) assertion
(let* ((*workspace* (make-workspace))
       (buffer (make-buffer :string "(is = (* 2 3) x)"
                            :point 1))
       ($node (node-iterator buffer)))
  (progn ;; list
    ;; (node-string $node)
    ;; T
    ;; (match (compile-pattern `(:symbol "IS")) $node)
    #++ (node-string
         (to
          (find-binding (match (compile-pattern `((:symbol "IS") ?x)) $node) '?x)))
    (breeze.analysis::with-match-let ($node ((:symbol "IS" #++ "PARACHUTE")
                                             ?cmp ?expected (:var ?got (:maybe :_))))
      #++ (loop :for (from . to) :in (breeze.test.pattern::bindings-alist bindings)
            :collect (cons from (node-string to)))
      (list ?cmp ?expected ?got)
      (mapcar 'node-string (list ?cmp ?expected ?got))
      bindings)))

(let* ((*workspace* breeze.test.workspace:*breeze-workspace*)
       (buffer (find-buffer "tests/documentation.lisp"))
       (needle ))
  (search "(is string=" (source buffer)))

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
