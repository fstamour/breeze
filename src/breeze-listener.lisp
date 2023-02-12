(in-package #:common-lisp-user)

(defpackage #:breeze.listener
  (:use :cl #:alexandria #:breeze.command)
  (:documentation "Swank/Slynk wrapper")
  (:import-from #:breeze.xref
                #:classp)
  (:import-from #:breeze.utils
                #:optimal-string-alignment-distance*)
  (:export
   #:interactive-eval
   #:interactive-eval-command
   #:get-recent-interactively-evaluated-forms))

(in-package #:breeze.listener)



(defparameter *recent-forms* ()
  "A list of recently-evaluated forms (as strings).")

;; TODO Use a heap to get the N smallest values!
;; TODO Put that into utils?
(defmacro minimizing ((var
                       &key
                         (score-var (gensym "score"))
                         tracep)
                      &body body)
  "Creates both a variable (let) and a function (flet) to keep track
of the instance of that had the smallest score."
  (check-type var symbol)
  `(let  ((,var nil)
          (,score-var))
     (flet ((,var (new-candidate new-score)
              ,@(when tracep
                  `((format *debug-io* "~&new-candidate: ~s new-score: ~s"
                            new-candidate new-score)))
              (when (and new-score
                         (or
                          ;; if it wasn't initialized already
                          (null ,var)
                          ;; it is initialized, but score is better
                          (< new-score ,score-var)))
                (setf ,var new-candidate
                      ,score-var new-score))))
       ,@body
       (values ,var ,score-var))))


(defun find-most-similar-symbol (input)
  (minimizing (candidate)
    (do-symbols (sym)
      (when (fboundp sym)
        (candidate sym
                   (breeze.utils:optimal-string-alignment-distance*
                    input
                    (string-downcase sym)
                    3))))))

;; (find-most-similar-symbol "prin") ;; => princ, 1

(defun find-most-similar-package (input)
  (minimizing (candidate)
    (loop :for package in (list-all-packages)
          :for package-name = (package-name package) :do
            (loop :for name in `(,package-name ,@(package-nicknames package)) :do
              (candidate name
                         (breeze.utils:optimal-string-alignment-distance*
                          input
                          (string-downcase name)
                          3))))))

#+ (or)
(progn
  (find-most-similar-package "breeze.util")
  ;; => breeze.utils, 1

  (find-most-similar-package "commmon-lisp")
  ;; => "COMMON-LISP", 1
  )

(defun find-most-similar-class (input)
  (minimizing (candidate)
    (do-symbols (sym)
      (when (classp sym)
        (candidate sym
                   (breeze.utils:optimal-string-alignment-distance*
                    input
                    (string-downcase sym)
                    3))))))

(defvar *last-invoked-restart* nil
  "For debugging purposes only")

(defun resignal-with-suggestion-restart (input candidate condition)
  ;; Ok, this is messy as hell, but it works
  (unless
      ;; We install a new restart
      (with-simple-restart (use-suggestion
                            "Use \"~a\" instead of \"~a\"."
                            candidate input)
        ;; with-simple-restart returns the _last evaluated_ form
        t
        ;; Then we signal the condition again
        (error condition))
    ;; with-simple-restart will return nil and t if the restart was
    ;; invoked
    (let ((use-value (find-restart 'use-value condition)))
      (setf *last-invoked-restart* (list candidate))
      (format *debug-io* "~&About to invoke the restart ~s with the value ~s."
              use-value
              candidate)
      ;; (describe use-value)
      ;; (inspect use-value)
      (invoke-restart use-value candidate))))

(defun suggest (input candidate condition)
  (message "Did you mean \"~a\"?" candidate)
  (when candidate
    (let ((restart (find-restart 'use-value condition)))
      (or
       (and restart (resignal-with-suggestion-restart
                     input candidate condition))
       (warn "Did you mean \"~a\"?~%~a"
             candidate
             (breeze.utils:indent-string
              2
              (breeze.utils:print-comparison
               nil
               (string-downcase candidate)
               input)))))))

(defgeneric condition-suggestion-input (condition)
  (:documentation "Get input for \"find-most-similar-*\" functions from a condition")
  ;; Default implementation
  (:method (condition)
    (cell-error-name condition))
  (:method ((condition undefined-function))
    (format *debug-io* "~&1")
    (cell-error-name condition))
  (:method ((condition package-error))
    (let ((package-designator
            (package-error-package condition)))
      (if (stringp package-designator)
          package-designator
          #+sbcl ;; only tested on sbcl
          (car
           (slot-value condition
                       'sb-kernel::format-arguments)))))
  #+sbcl
  (:method ((condition sb-ext:package-does-not-exist))
    (package-error-package condition))
  #+sbcl
  (:method ((condition sb-pcl:class-not-found-error))
    (sb-kernel::cell-error-name condition)))

;; (trace condition-suggestion-input)

(defmacro defun-suggest (types)
  `(progn
     ,@(loop
         :for type :in types
         :collect
         `(defun ,(symbolicate 'suggest- type) (condition)
            (let* ((input (string-downcase (condition-suggestion-input condition)))
                   (candidate (,(symbolicate 'find-most-similar- type) input)))
              #+ (or)
              (format *debug-io*
                      ,(format nil
                               "~~&candidate ~(~a~): ~~s"
                               type)
                      candidate)
              (if candidate
                  (suggest input candidate condition)
                  (error condition)))))))

(defun-suggest
    (symbol
     package
     class))

#+ (or)
(trace suggest-symbol
       suggest-package
       suggest-class)

#+ (or)
(progn
  ;; List the slot of a condition
  (sb-kernel::condition-assigned-slots *condition*)

  ;; Get the first element of a condition's format arguments
  (car
   (slot-value *condition*
               'sb-kernel::format-arguments)) )

(defvar *last-condition* nil
  "For debugging purposose only.")

#+ (or)
(defparameter *condition* *last-condition*
  "Just a quick way to save the last-condition.")

#+ (or)
(type-of *condition*)
;; => SB-PCL::MISSING-SLOT

(defun call-with-correction-suggestion (function)
  "Funcall FUNCTION wrapped in a handler-bind form that suggest corrections."
  (handler-bind
      ((error #'(lambda (condition)
                  (setf *last-condition* condition)
                  (error condition))))
    (handler-bind
        ;; The order is important!!!
        ((undefined-function #'suggest-symbol)
         #+sbcl (sb-ext:package-does-not-exist #'suggest-package)
         #+sbcl (sb-int:simple-reader-package-error #'suggest-symbol)
         #+ (or)
         (package-error #'suggest-package)
         #+sbcl
         (sb-pcl:class-not-found-error #'suggest-class))
      (funcall function))))

;; (prin t)
;; (commmon-lisp:print :oups)
;; (cl:prin :oups)
;; (call-with-correction-suggestion (lambda () (eval '(prin))))
;; (make-instance 'typos)


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

;; TODO Use serapeum:run-hook
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
       (%interactive-eval substring)
       (run-interactive-eval-after-hooks string substring)
       ))))

(defparameter *interactive-eval-last-context* ()
  "For debugging only.")

(define-command interactive-eval-command ()
  "A command to interactively evaluate code."
  (let ((context (command-context*)))
    (setf *interactive-eval-last-context* context)
    (if (augment-context-by-parsing-the-buffer context)
        (let ((node (context-get context
                                 ;; 'breeze.command::parent-node
                                 'breeze.command::outer-node)))
          (progn
            ;; TODO (pulse-momentary-highlight-region begin end)
            ;; TODO Find what's the value of *package* at this node...
            (let ((string (breeze.syntax-tree:node-raw node)))
              (interactive-eval string)
              ;; (message "~s" string)
              )))
        ;; (message "Can't parse maybe?")
        )))


;;; Evaluation history

;; TODO cleanup *recent-forms* from time to time.
;; TODO maybe uses a hash-table instead of a list for *recent-forms* to keep a
;;  kind of frequency-table

(defun get-recent-interactively-evaluated-forms ()
  "Get the 50 most recently evaluated forms"
  (loop :for form :in *recent-forms*
        :for i :below 50
        :do (format t "~&~a~%"
                    (remove #\newline form))))


;;; Thread management

(defun find-worker-threads ()
  (let ((current-thread (bt:current-thread)))
    (remove-if-not #'(lambda (thread)
                       (and (not (eq current-thread thread))
                            (string= "worker" (bt:thread-name thread))))
                   (sb-thread:list-all-threads))))

(defun kill-worker-threads ()
  (let ((threads (find-worker-threads)))
    (when threads
      (mapcar #'bordeaux-threads:destroy-thread threads))
    (format t "Killed ~d threads." (length threads))))

#|
(kill-worker-threads)
|#
