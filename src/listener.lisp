(in-package #:common-lisp-user)

(defpackage #:breeze.listener
  (:use :cl #:alexandria #:breeze.command)
  (:documentation "RPC, REPL and more.")
  (:import-from #:breeze.xref
                #:classp
                #:function-designator-p)
  (:import-from #:breeze.string
                #:optimal-string-alignment-distance*)
  (:export
   #:rpc-eval
   #:interactive-eval
   #:interactive-eval-command
   #:get-recent-interactively-evaluated-forms))

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

(defun %interactive-eval (string)
  ;; &optional package readtable
  ;; TODO infer the *package*
  ;; TODO infer the *readtable*
  (let ((*package* (find-package "CL-USER"))
        (*readtable* (cl:copy-readtable nil))
        (values (multiple-value-list
                 (eval
                  (read-from-string string)))))
    (message (format-values-for-echo-area values))))

(defun run-interactive-eval-after-hooks (string substring)
  (loop
    :for (name . hook) :in *interactive-eval-hooks*
    :do
       (handler-case
           (funcall hook string substring)
         (error (condition)
           (format *error-output*
                   "~&Error signaled while running \"~a\" interactive-eval hook: ~a~%  "
                   name
                   condition)))))

(defun interactive-eval (string)
  "Interactively evaluate a string."
  (pushnew string *recent-forms* :test #'string=)

  ;; TODO add a restart to retry
  ;; (format t "~&Interactive-eval: ~A~%" string)
  (let ((substring string))
    (call-with-correction-suggestion
     (lambda ()
       (prog1
           (%interactive-eval substring)
         (run-interactive-eval-after-hooks string substring))))))

(defparameter *interactive-eval-last-context* ()
  "For debugging only.")

(defparameter *last-parse* nil
  "I sure love this trick!")

(define-command interactive-eval-command ()
  "A command to interactively evaluate code."
  (setf *last-parse* (breeze.lossless-reader:parse (buffer-string)))
  (message "Parsed without signalling an error.")
  #++
  (let ((context (context*)))
    (setf *interactive-eval-last-context* context)
    (if (augment-context-by-parsing-the-buffer context)
        (let ((node (context-get context
                                 ;; 'breeze.command::parent-node
                                 'breeze.command::outer-node)))
          (progn
            ;; TODO (pulse-momentary-highlight-region begin end)
            ;; TODO Find what's the value of *package* at this node...
            #++
            (let ((string (breeze.syntax-tree:node-raw node)))
              (interactive-eval string)
              ;; (message "~s" string)
              )))
        ;; (message "Can't parse maybe?")
        )))
