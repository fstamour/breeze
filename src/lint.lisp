(defpackage #:breeze.lint
  (:documentation "Commands for linting and automatic fixes")
  (:use #:cl #:breeze.analysis #:breeze.command)
  (:import-from #:breeze.buffer
                #:buffer)
  (:import-from #:breeze.package
                #:in-package-node-p)
  (:export #:target-node
           #:replacement)
  (:export
   #:lint-buffer
   #:fix-buffer)
  (:export #:lint))

(in-package #:breeze.lint)


;;; Utilities to collect "diagnostics"

(defvar *diagnostics* nil)
(defvar *point-max* nil)

(defun make-diagnostic (start end severity format-string format-args)
  "Create a \"diagnostic\" object."
  (list start (if (= +end+ end) *point-max* end)
        severity
        (apply #'format nil format-string format-args)))

(defun push-diagnostic* (start end severity format-string format-args)
  "Create a diagnostic object and push it into the special variable
*diagnostics*."
  (let ((diagnostic (make-diagnostic start end
                                     severity
                                     format-string format-args)))
    (push diagnostic *diagnostics*)
    diagnostic))

;; Same as push-diagnostic*, but takes a &rest
(defun push-diagnostic (start end severity format-string &rest format-args)
  "Create a diagnostic object and push it into the special variable
*diagnostics*."
  (push-diagnostic* start end severity format-string format-args))

(defun diag-node (node severity format-string &rest format-args)
  "Create a diagnostic object for NODE and push it into the special
variable *diagnostics*."
  (push-diagnostic* (start node) (end node)
                    severity format-string format-args))

(defun diag-warn (node format-string &rest format-args)
  "Create a diagnostic object for NODE with severity :WARNING and push it
into the special variable *diagnostics*."
  (apply #'diag-node node :warning format-string format-args))

(defun diag-error (node format-string &rest format-args)
  "Create a diagnostic object for NODE with severity :error and push it
into the special variable *diagnostics*."
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


;;; Linter rules

#++
(defun check-in-package ()
  "Make sure the previous in-package form desginates a package that can
be found. If it's not the case (e.g. because the user forgot to define
a package and/or evaluate the form that defines the package) they show
a message and stop the current command."
  (let ((package (current-package)))
    (if package
        (let ((package-name (breeze.analysis::node-string-designator-string
                             package)))
          (unless (find-package package-name)
            (message "The nearest in-package form designates a package that doesn't exists: ~s"
                     package-name)
            (return-from-command)))
        (;; TODO message no (in-package ...) found...
         (return-from-command)))))


;; TODO use a node-iterator instead
;; TODO this is _very_ similar to warn-undefined-in-package and check-in-package
;; TODO rename validate current-package
#++
(defun validate-nearest-in-package (nodes outer-node)
  "Find the lastest \"in-package\" form, test if the packages can be
found."
  (let* ((previous-in-package-form
           (find-nearest-sibling-in-package-form nodes (or outer-node
                                                           (point)))))
    (when previous-in-package-form
      (let* ((package-designator (in-package-node-package
                                  previous-in-package-form))
             (package (find-package package-designator)))
        (when (null package)
          package-designator)))))

;; TODO this rule only make sense when "in-image"
(defun warn-undefined-in-package (node-iterator)
  (alexandria:when-let* ((package-designator-node (in-package-node-p node-iterator))
                         (package-name (node-string-designator-string
                                        package-designator-node)))
    (unless (find-package package-name)
      (breeze.lint::node-style-warning
       node-iterator
       (format nil "Package ~s is not currently defined." package-name)))))


(defun warn-extraneous-whitespaces (node-iterator)
  (let ((firstp (firstp node-iterator))
        (lastp (lastp node-iterator)))
    (cond
      ((and firstp lastp)
       (node-style-warning
        node-iterator "Extraneous whitespaces."
        :replacement nil))
      (firstp
       (node-style-warning
        node-iterator "Extraneous leading whitespaces."
        :replacement nil))
      ((and lastp (not (line-comment-node-p
                        (previous-sibling node-iterator))))
       (node-style-warning
        node-iterator "Extraneous trailing whitespaces."
        :replacement nil))
      ((and (not (or firstp lastp))
            ;; Longer than 1
            (< 1 (- (end node-iterator) (start node-iterator)))
            ;; "contains no newline"
            (not (position #\Newline
                           (source node-iterator)
                           :start (start node-iterator)
                           :end (end node-iterator)))
            ;; is not followed by a line comment
            (not (line-comment-node-p
                  (next-sibling node-iterator))))
       (node-style-warning
        node-iterator "Extraneous internal whitespaces."
        :replacement " ")))))

;; TODO detect #:package:symbol => package:symbol

(defun error-invalid-node (node-iterator)
  (unless (valid-node-p (value node-iterator))
    (node-parse-error node-iterator "Syntax error")))

#++ ;; TODO make the list of "linting rules" dynamic
(defvar *rules*
  (list
   'error-invalid-node
   'warn-undefined-in-package
   '((and (plusp depth)
      (whitespace-node-p node))
     'warn-extraneous-whitespaces)))

(defun analyse (buffer
                &aux (node-iterator (make-node-iterator buffer)))
  "Apply all the linting rules."
  (check-type buffer buffer)
  ;; TODO warn when using double colon for external symbols (e.g. cl::defun)
  (loop :until (donep node-iterator)
        :for node = (value node-iterator)
        :for depth = (slot-value node-iterator 'depth)
        :do (progn
              (error-invalid-node node-iterator)
              ;; (warn-undefined-in-package node-iterator)
              (when (and (plusp depth)
                         (whitespace-node-p node))
                (warn-extraneous-whitespaces node-iterator)))
            (next node-iterator)))

(defun lint-buffer (buffer &aux (*diagnostics* '()))
  "Apply all the linting rules, and accumulate the \"diagnostics\"."
  (check-type buffer buffer)
  (handler-bind
      ((node-parse-error (lambda (condition)
                           (diag-error (target-node condition)
                                       (simple-condition-format-control condition)
                                       (simple-condition-format-arguments condition))
                           (return-from lint-buffer *diagnostics*)))
       (node-style-warning (lambda (condition)
                             (diag-warn (target-node condition)
                                        (simple-condition-format-control condition)
                                        (simple-condition-format-arguments condition)))))
    (analyse buffer))
  *diagnostics*)

(defun fix-buffer (buffer)
  (check-type buffer buffer)
  (uiop:while-collecting (conditions)
    (handler-bind ((simple-node-condition
                     (lambda (condition)
                       (when (replacementp condition)
                         (conditions condition)))))
      (analyse buffer))))


(define-command lint ()
  "Lint the current buffer."
  (return-value-from-command
   (lint-buffer (current-buffer))))
