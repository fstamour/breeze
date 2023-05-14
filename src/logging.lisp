(defpackage #:breeze.logging
  (:documentation "Utilities for logging.")
  (:use #:cl)
  (:export
   #:log-message
   #:log-critical
   #:log-error
   #:log-warning
   #:log-info
   #:log-debug))

(in-package #:breeze.logging)

;; TODO These could probably profit from being methods instead of
;; plain functions.

(defun log-level ()
  "Get the current log level."
  ;; TODO This is just the minimal thing I could do to make it work
  :info)

(defun log-stream ()
  "Get the current log output stream."
  *debug-io*)

(defun compare-level (cmp level1 level2)
  "Compare two log levels."
  ;; It's implemented in a way to have a total order, without actually
  ;; assigning a value to each level.
  (funcall cmp
           (length (member level1 #1='(:debug :infor :warning :error :critical)))
           (length (member level2 #1#))))

#++
(list
 (compare-level #'< :debug :debug)
 (compare-level #'< :debug :critical)
 (compare-level #'< :critical :debug))

(defun log-message (level control-string &rest args)
  (let ((current-level (log-level))
        (stream (log-stream)))
    (when (compare-level #'>= level current-level)
      ;; TODO maybe print the time too?
      (format stream "~&~A " level)
      (apply #'format stream control-string args))))


(macrolet ((def (level)
             `(defun ,(alexandria:symbolicate 'log- level) (control-string &rest args)
                (apply #'log-message ,level control-string args))))
  (def :critical)
  (def :error)
  (def :warning)
  (def :info)
  (def :debug))
