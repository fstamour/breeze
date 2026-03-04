(defpackage #:breeze.diagnostics
  (:documentation "Utilities for creating, collecting, etc. diagnostics.")
  (:use #:cl)
  (:import-from #:breeze.parser
                #:node-iterator
                #:+end+
                #:start
                #:end
                #:copy-iterator)
  (:export #:scan
           #:diagnostics
           #:livep
           #:*scan*
           #:livep*
           #:make-diagnostic
           #:push-diagnostic
           #:push-diagnostic*
           #:diag-node
           #:diag-warn
           #:diag-error
           #:simple-node-condition
           #:node-iterator
           #:target-node
           #:replacement
           #:replacementp
           #:simple-node-error
           #:simple-node-warning
           #:node-parse-error
           #:node-style-warning))

(in-package #:breeze.diagnostics)


;;; Utilities to collect "diagnostics"

(defclass scan ()
  ((diagnostics
    :initform nil
    :initarg :diagnostics
    :accessor diagnostics
    :documentation "Collect all the diagnostics")
  (livep
    :initform t
    :initarg :livep
    :accessor livep
    :documentation "True if the scan is done for a live session (for example, for n editor
connected to an image through slime or sly)."))
  (:documentation "A run of the linter and the diagnistics that were found."))

(defparameter *scan* nil
  "The current run of the linter. Holds the linter's configurations and keeps track of all the diagnostics.")

(defun livep* ()
  (livep *scan*))



(defvar *point-max* nil)

(defun make-diagnostic (start end severity format-string format-args)
  "Create a \"diagnostic\" object."
  (list start (if (= +end+ end) *point-max* end)
        severity
        (apply #'format nil format-string format-args)))

(defun push-diagnostic* (start end severity format-string format-args)
  "Create a diagnostic object and push it into the special variable
*scan*."
  (let ((diagnostic (make-diagnostic start end
                                     severity
                                     format-string format-args)))
    (push diagnostic (diagnostics *scan*))
    diagnostic))

;; Same as push-diagnostic*, but takes a &rest
(defun push-diagnostic (start end severity format-string &rest format-args)
  "Create a diagnostic object and push it into the special variable
*scan*."
  (push-diagnostic* start end severity format-string format-args))

(defun diag-node (node severity format-string &rest format-args)
  "Create a diagnostic object for NODE and push it into the special
variable *scan*."
  (push-diagnostic* (start node) (end node)
                    severity format-string format-args))

;; TODO severity :note
;; TODO severity :style-warning

(defun diag-warn (node format-string &rest format-args)
  "Create a diagnostic object for NODE with severity :WARNING and push it
into the special variable *scan*."
  (apply #'diag-node node :warning format-string format-args))

(defun diag-error (node format-string &rest format-args)
  "Create a diagnostic object for NODE with severity :error and push it
into the special variable *scan*."
  (apply #'diag-node node :error format-string format-args))


;;; using signals to be able to decide what to do when certain
;;; conditions are discovered (e.g. keep note of it v.s. fixing it)

#|

Which conditions should I use?

simple-error
simple-warning

parse-error
style-warning

simple-condition-format-control, simple-condition-format-arguments

(apply #'format nil (simple-condition-format-control foo)
(simple-condition-format-arguments foo))

|#

(define-condition simple-node-condition (simple-condition)
  ((node-iterator :initarg :node-iterator :reader target-node)
   (replacement :initarg :replacement :reader replacement :initform 'cl:null)))

(defun replacementp (simple-node-condition)
  (not (eq (replacement simple-node-condition) 'cl:null)))

(define-condition simple-node-error (simple-node-condition simple-error) ())

(define-condition simple-node-warning (simple-node-condition simple-warning) ())

(define-condition node-parse-error (simple-node-error parse-error) ())

(defun node-parse-error (node-iterator message &key (replacement 'null))
  (signal (make-condition
           'node-parse-error
           :node-iterator (copy-iterator node-iterator)
           :format-control message
           :replacement replacement)))

(define-condition node-style-warning (simple-node-warning style-warning) ())

(defun node-style-warning (node-iterator message &key (replacement 'null))
  (signal (make-condition
           'node-style-warning
           :node-iterator (copy-iterator node-iterator)
           :format-control message
           :replacement replacement)))

#++
(let ((c (node-style-warning (node 'dummy 0 1) "Bad node ~a")))
  (apply #'format nil (simple-condition-format-control c)
         (target-node c)
         (simple-condition-format-arguments c)))
