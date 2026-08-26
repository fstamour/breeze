(defpackage #:breeze.lint
  (:documentation "Commands for linting and automatic fixes")
  (:use #:cl
        #:breeze.analysis
        #:breeze.command
        #:breeze.diagnostics
        #:breeze.checks)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:import-from #:breeze.buffer
                #:buffer)
  (:export #:target-node
           #:replacement)
  (:export #:analyse
           #:lint-buffer
           #:fix-buffer)
  (:export #:lint))

(in-package #:breeze.lint)

(defun error-invalid-node (node-iterator)
  (unless (valid-node-p (value node-iterator))
    (let* ((node (value node-iterator))
           (errors (errors node)))
      (if errors
          (node-parse-error node-iterator
                            (with-output-to-string (out)
                              (loop :for error :in errors
                                    :do (fresh-line out) (apply #'format out error))))
          (node-parse-error node-iterator "Syntax error")))))

(defun analyse (buffer &aux (*checks* *checks*))
  "Apply all the linting rules on BUFFER."
  (check-type buffer buffer)
  (destructuring-bind (&key top-level-checks by-node-types by-not-node-types)
      (group-checks *checks*)
    (loop
      :with $node = (make-node-iterator buffer)
      :until (tree-iterator-done-p $node)
      :for node = (tree-iterator-value $node)
      :for node-type = (node-type node)
      ;; :for previous-node := nil :then node
      :for depth = (depth $node)
      :for i :from 0
      :do
         (when (<= 1000000 i)
           (break "Infinite loop? in (lint:analyse)"))
         ;; if the current node has any syntax error, signal them
         (error-invalid-node $node)
         ;; Apply all the checks!
         ;; TODO this should be top-level-p (instead of (zerop depth))
         (when (zerop depth)
           (map nil #1=(lambda (fn)
                         (funcall fn $node :buffer buffer))
                top-level-checks))
         (map nil #1# (gethash node-type by-node-types))
         (maphash (lambda (type fns)
                    (unless (eq node-type type)
                      (map nil #1# fns)))
                  by-not-node-types)
         (next-preorder $node))))

(defun lint-buffer (buffer
                    &key
                      (livep t)
                      (*scan* (make-instance 'scan :livep livep)))
  "Apply all the linting rules on BUFFER, and accumulate the \"diagnostics\"."
  (check-type buffer buffer)
  ;; TODO &key livep and scan are mutually exclusive
  (handler-bind
      ((node-parse-error
         (lambda (condition)
           (diag-error (target-node condition)
                       (simple-condition-format-control condition)
                       (simple-condition-format-arguments condition))
           ;; For syntax error, we stop here
           (return-from lint-buffer (diagnostics *scan*))))
       (simple-node-note
         (lambda (condition)
           (diag-note (target-node condition)
                      (simple-condition-format-control condition)
                      (simple-condition-format-arguments condition))))
       (simple-node-warning
         (lambda (condition)
           (diag-warn (target-node condition)
                      (simple-condition-format-control condition)
                      (simple-condition-format-arguments condition))))
       (simple-node-error
         (lambda (condition)
           (diag-error (target-node condition)
                       (simple-condition-format-control condition)
                       (simple-condition-format-arguments condition)))))
    (analyse buffer))
  (diagnostics *scan*))

;; TODO currently, this re-analyze the buffer and collects all the
;; fixable issues. the issues should be cached (probably in the
;; workspace object).
(defun fix-buffer (buffer &aux (*scan* (make-instance 'scan :livep nil)))
  (check-type buffer buffer)
  (uiop:while-collecting (conditions)
    (handler-bind ((simple-node-condition
                     (lambda (condition)
                       (when (replacementp condition)
                         (conditions condition)))))
      (analyse buffer))))


(define-command lint ()
  "Lint the current buffer."
  (declare noninteractive)
  (return-value-from-command
   (lint-buffer (current-buffer))))
