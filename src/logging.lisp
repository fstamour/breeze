(defpackage #:breeze.logging
  (:documentation "Utilities for logging.")
  (:use #:cl)
  (:export
   #:log-level
   #:log-message
   #:log-critical
   #:log-error
   #:log-warning
   #:log-info
   #:log-debug))

(in-package #:breeze.logging)

(defparameter *log-level* :info
  "The current log level")

(defun log-level () "Get the current log level." *log-level*)

(defun (setf log-level) (new-value)
  (check-type new-value (member :critical :error :warning :info :debug))
  (setf *log-level* new-value))

(defun log-stream ()
  "Get the current log output stream."
  *trace-output*)

(defun compare-level (cmp level1 level2)
  "Compare two log levels."
  ;; It's implemented in a way to have a total order, without actually
  ;; assigning a value to each level.
  (funcall cmp
           (length (member level1 #1='(:critical :error :warning :info :debug)))
           (length (member level2 #1#))))

(defun log-message (level control-string &rest args)
  (let ((current-level (log-level))
        (stream (log-stream)))
    (when (compare-level #'>= level current-level)
      ;; TODO maybe print the time too?
      (format stream "~&~a ~?~%" level control-string args))))


(macrolet ((def (level)
             `(defun ,(alexandria:symbolicate 'log- level) (control-string &rest args)
                (apply #'log-message ,level control-string args))))
  (def :critical)
  (def :error)
  (def :warning)
  (def :info)
  (def :debug))

;; To manually test if the log level is respected.
#++
(progn
  (format (log-stream) "~%~%Current log level: ~s~%~%" (log-level))
  (log-debug "debug")
  (log-info "info")
  (log-warning "warn")
  (log-error "err")
  (log-critical "crit"))
