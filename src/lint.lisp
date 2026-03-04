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
  (:export #:lint-buffer
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

(defun analyse (buffer
                &aux
                  (*checks* *checks*))
  "Apply all the linting rules on BUFFER."
  (check-type buffer buffer)
  (loop
    :with node-iterator = (make-node-iterator buffer)
    :with source := (source buffer)
    :with checks := (alexandria:hash-table-values *checks*)
    :until (donep node-iterator)
    :for node = (value node-iterator)
    ;; :for previous-node := nil :then node
    :for depth = (slot-value node-iterator 'depth)
    :do
       ;; if the current node has any syntax error, signal them
       (error-invalid-node node-iterator)
       ;; Apply all the checks!
       (map 'nil (lambda (check)
                   (apply-check check node-iterator
                                ;; TODO this should be top-level-p (instead of (zerop depth)
                                :top-level-p (zerop depth)))
            checks)
       (next-preorder node-iterator)))

(defun lint-buffer (buffer
                    &key
                      (livep t)
                      (*scan*
                       (make-instance 'scan
                                      :livep livep)))
  "Apply all the linting rules on BUFFER, and accumulate the \"diagnostics\"."
  (check-type buffer buffer)
  ;; TODO &key livep and scan are mutually exclusive
  (handler-bind
      ((node-parse-error (lambda (condition)
                           (diag-error (target-node condition)
                                       (simple-condition-format-control condition)
                                       (simple-condition-format-arguments condition))
                           (return-from lint-buffer (diagnostics *scan*))))
       (node-style-warning (lambda (condition)
                             (diag-warn (target-node condition)
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
