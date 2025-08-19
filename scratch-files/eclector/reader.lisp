(in-package #:common-lisp-user)

(uiop:define-package #:breeze.reader
    (:documentation
     "Parser for common lisp that doesn't lose any information.

This package also re-exports symbols from breeze.syntax-tree.")
  (:use :cl)
  (:use-reexport #:breeze.syntax-tree)
  (:import-from #:alexandria
                #:if-let
                #:when-let)
  (:import-from #:breeze.string
                #:subseq-displaced)
  (:export
   ;; Parse and unparse list of forms
   #:parse
   #:parse-string
   #:unparse-to-stream
   #:unparse-to-string
   #:forms
   #:parser-conditon))

(in-package #:breeze.reader)


;;; Data structures (there's even more in "syntax-tree.lisp")

;; TODO Find a better name... anything better
(defclass code ()
  ((forms
    :initform nil
    :initarg :forms
    :accessor forms
    :documentation "List of forms (syntax nodes)")
   (parser-condition
    :initform nil
    :initarg :condition
    :accessor parser-condition
    :documentation "The condition that occurred while parsing"))
  (:documentation "Represents a parsed piece of code."))

(defun make-code (&optional forms)
  (make-instance 'code
                 :forms forms))


(defun raw (breeze-client start end)
  (let* ((source (source breeze-client))
         (end (min end (length source))))
    (subseq-displaced source start end)))

(defun add-offset (client source)
  (incf (car source) (start client))
  (incf (cdr source) (start client)))


;; Local Nicknames
(defmethod eclector.reader:interpret-symbol
    ((client breeze-client) (stream t)
     package-indicator symbol-name internp)
  (unless (case package-indicator
            (:current *package*)
            (:keyword (find-package "KEYWORD"))
            (t (find-package package-indicator)))
    (if-let (actual-package
             (cdr (assoc package-indicator
                         (trivial-package-local-nicknames:package-local-nicknames *package*)
                         :test #'string=)))
      (setf package-indicator actual-package)))
  (call-next-method))


;; #.
(defmethod eclector.reader:evaluate-expression ((client breeze-client)
                                                expression)
  "Create a syntax node for #. ."
  (make-instance 'read-eval-node :content expression))


;; #+ and #-
(defmethod eclector.reader:evaluate-feature-expression
    ((client breeze-client) feature-expression)
  (if (typep feature-expression 'node)
      ;; This is very hacky ¯\_(ツ)_/¯
      (eclector.reader:call-with-current-package
       client
       #'(lambda ()
           (eclector.reader:evaluate-feature-expression
            client
            (read-from-string (node-raw feature-expression))))
       :keyword)
      (call-next-method)))


;; #\
(defmethod eclector.reader:find-character ((client breeze-client)
                                           designator)
  "Create a syntax node for #\\ ."
  (make-instance 'character-node
                 :content (format nil "#\\~a" designator)
                 :char
                 (call-next-method)))


;; _All_ reader macros
(defmethod eclector.reader:call-reader-macro ((client breeze-client)
                                              stream char
                                              readtable)
  "Create a syntax node for lists."
  (case char
    (#\(
     (make-instance 'list-node
                    :content (call-next-method)))
    (t
     (call-next-method))))


;; #'
(defmethod eclector.reader:wrap-in-function ((client breeze-client)
                                             name)
  "Create a syntax node for #' ."
  (make-instance 'function-node :content name))



;;; Reading multiple forms = ""parsing""

(defun read-all-forms (string)
  (let ((eof (gensym "eof"))
        (client (make-instance
                 'breeze-client
                 :source string)))
    (values
     (loop
       :with cursor = 0
       :for (form position)
         = (multiple-value-list
            (progn
              (setf (start client) cursor)
              (eclector.parse-result:read-from-string
               client
               string
               ;; Don't error on eof
               nil
               ;; Return eof instead
               eof
               :start cursor
               :preserve-whitespace t)))
       :while (not (eq eof form))
       :do (setf cursor position)
       :when (not (eq eof form))
         :collect form)
     client)))


(defun parse-string (string &optional errorp)
  "Read STRING entirely using breeze's reader."
  ;; (read-all-forms string)
  ;; #+ (or)
  (handler-bind
      ((error #'(lambda (condition)
                  (unless errorp
                    (return-from parse-string
                      (make-instance 'code :condition condition))))))
    (make-code (read-all-forms string))))
