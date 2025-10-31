(in-package #:common-lisp-user)

(defpackage #:breeze.listener
  (:documentation "RPC, REPL and more.")
  (:use :cl #:breeze.analysis #:breeze.command)
  (:import-from #:alexandria
                #:when-let*
                #:when-let
                #:symbolicate #| used in suggestions.lisp |#)
  (:import-from #:breeze.xref
                #:classp
                #:function-designator-p)
  (:import-from #:breeze.string
                #:optimal-string-alignment-distance*)
  (:import-from #:breeze.buffer
                #:current-package
                #:point)
  (:export
   #:rpc-eval
   #:interactive-eval-string
   #:interactive-eval))

(in-package #:breeze.listener)



(defun rpc-eval (string)
  "Low-level eval used as a kind of RPC. We need this to avoid issue from
differences between swank and slynk."
  (let ((*package* (find-package "CL-USER"))
        (*readtable* (cl:copy-readtable nil)))
    (eval (read-from-string string))))


(defparameter *interactive-eval-hooks* '())

;; TODO Use with-timeout

(defun format-values-for-echo-area (values)
  (let ((*print-readably* nil))
    (cond ((null values) "; No value")
          ((and (integerp (car values)) (null (cdr values)))
           (let ((i (car values)))
             (format nil "~D (~a bit~:p, #x~X, #o~O, #b~B)"
                     i (integer-length i) i i i)))
          ((and (typep (car values) 'ratio)
                (null (cdr values))
                (ignore-errors
                 ;; The ratio may be too large to be represented as
                 ;; a single float
                 (format nil "~D (~:*~f)"

                         (car values)))))
          (t (format nil "~{~S~^, ~}" values)))))

(defun interactive-eval-string (string)
  ;; TODO maybe keep an history (pushnew string *recent-forms* :test #'string=)
  ;; TODO add a restart to retry
  (call-with-correction-suggestion
   (lambda ()
     (prog1
         ;; &optional package readtable
         ;; TODO infer the *readtable*

         (let ((*readtable* (cl:copy-readtable nil))
               (values (multiple-value-list
                        (eval
                         (read-from-string string)))))
           (message "~a" (format-values-for-echo-area values)))
       (run-interactive-eval-after-hooks string)))))

(defun run-interactive-eval-after-hooks (string)
  (loop
    :for (name . hook) :in *interactive-eval-hooks*
    :do
       (handler-case
           (funcall hook string)
         (error (condition)
           (format *error-output*
                   "~&Error signaled while running \"~a\" interactive-eval hook: ~a~%  "
                   name
                   condition)))))

;; TODO eval-node
;;  that would be useful to implement a command for updating tests

(defparameter *interactive-eval-last-context* ()
  "For debugging only.")

#++
(defparameter *current-node* nil
  "The node iterator currently being evaluated.")

;; 2025-06-12 it finally works!
(define-command interactive-eval ()
  "A command to interactively evaluate code."
  (let* ((context (context*))
         (buffer (current-buffer context))
         ($node (node-iterator buffer))
         ($package (current-package buffer))
         (*package* *package*))
    ;; TODO if the form is a comment
    (when (whitespace-or-comment-node-p $node)
      ;; TODO find the closest node to evaluate
      ;; TODO check if "firstp" and/or "lastp"
      (when-let ((previous (previous-sibling $node
                                             #| TODO add :skipp to previous-sibling |#)))
        (when (= (point buffer) (end previous))
          (decf (pos $node)))))
    ;; TODO add more to the "last context": the package, and the string to be evaluated
    (setf *interactive-eval-last-context* context)
    (when $package
      (when-let* ((package-name (breeze.analysis:node-string-designator $package)))
        (setf *package* (find-package package-name))))
    (when-let* (($node (root $node))
                (node (value $node)))
      ;; TODO use pulse-node
      (pulse (start node) (end node))
      (let ((string (node-string $node)))
        (interactive-eval-string string)
        ;; (message "~s" string)
        ))))
