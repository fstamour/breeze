(in-package #:common-lisp-user)

(uiop:define-package #:breeze.reader
  (:documentation
   "Parser for common lisp that doesn't lose any information.

This package also re-exports symbols from breeze.syntax-tree.")
  (:use :cl)
  (:use-reexport #:breeze.syntax-tree)
  (:import-from #:alexandria
                #:if-let)
  (:import-from #:breeze.utils
                #:subseq-displaced)
  (:export
   ;; Parse and unparse list of forms
   #:parse
   #:parse-string
   #:unparse-to-stream
   #:unparse-to-string))

(in-package #:breeze.reader)

;; (log:config '(breeze reader) :debug)


;;; Parser "client"

;; Define a class representing the "parse result client"
(defclass breeze-client (eclector.parse-result:parse-result-client)
  (
   ;; TODO We should probably rename this, to avoid confusion with the
   ;; node's source, which is a pair of index into this string.
   (source
    :initarg :source
    :type string
    :accessor source)
   (start
    :initarg :start
    :initform 0
    :type integer
    :accessor start))
  (:documentation
   "Controls how the reader construct the parse-results."))

(defun raw (breeze-client start end)
  (let* ((source (source breeze-client))
         (end (min end (length source))))
    (subseq-displaced source start end)))

(defun add-offset (client source)
  (incf (car source) (start client))
  (incf (cdr source) (start client)))


;;; hooks intro eclector

(defmethod eclector.parse-result:make-expression-result
    ((client breeze-client) (result t) (children t) (source t))
  "Create an expression result"
  (add-offset client source)
  (let* ((raw (raw client (car source) (cdr source)))
         (node
           (progn
             (log4cl:log-debug result children source raw)
             ;; (dbg "~&make-expression result: ~s children: ~s source: ~s raw: ~s" result children source raw)
             (cond
               ((or (alexandria:starts-with-subseq "#+" raw)
                    (alexandria:starts-with-subseq "#-" raw))
                ;; (let ((content (node-content))))
                (make-instance 'feature-expression-node
                               :feature-expression (first children)
                               :content
                               (if (nodep result)
                                   result ;;(node-content result)
                                   (cdr children))))
               (;; If result is a node, populate it
                (typep result 'node)
                (when children
                  (setf (node-content result) children))
                result)
               (;; If result is a symbol, make a symbol-node
                (symbolp result)
                (make-instance 'symbol-node
                               :content result))
               (;; If result is a stirng, make a string-node
                (stringp result)
                (make-instance 'string-node
                               :content result))
               (;; Else, make a generic node
                t
                (make-instance 'node
                               :content (or children result)))))))
    (setf (node-source node) source
          (node-raw node) raw)
    (log4cl:log-debug node)
    (when (in-package-form-p node)
      (log:debug "Changing the current package...")
      ;; TODO Add current-package to the client, to avoid changing the
      ;; user's current-package when reading
      ;; Will need to change the package-local-nickname logic
      (setf *package* (find-package (in-package-node-package node))))
    node))



;; Create a "make skipped input result" method for our custom client
(defmethod eclector.parse-result:make-skipped-input-result
    ((client breeze-client) (stream t) (reason t) (source t))
  "Create a skipped-node parse result."
  (add-offset client source)
  (let ((content (raw client (car source) (cdr source))))
    (log4cl:log-debug content)
    (make-instance 'skipped-node
                   :content content
                   :source source)))


;; Local Nicknames
(defmethod eclector.reader:interpret-symbol
    ((client breeze-client) (stream t)
     package-indicator symbol-name internp)
  (unless (case package-indicator
            (:current *package*)
            (:keyword (find-package "KEYWORD"))
            (t (find-package package-indicator)))
    (log:debug package-indicator symbol-name *package*)
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
  "Create a syntac node for #\\ ."
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



;;; Reading multiple forms + post processing = ""parsing""

(defun read-all-forms (string)
  (let ((eof (gensym "eof"))
        (client (make-instance
                 'breeze-client
                 :source string)))
    (values
     (loop
       :with cursor = 0
       :for (form position orphans)
         = (if (eq eof form)
               (return forms) ; Stop the loop here...
               (multiple-value-list
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
                   :preserve-whitespace t))))
       :do (setf cursor position)
       :if orphans
         :append (cond
                   ((eq eof form) orphans)
                   (t (sort
                       (append orphans (list form))
                       #'<
                       :key (lambda (node)
                              (car (node-source node))))))
           :into forms
       :else
         :when (not (eq eof form))
           :collect form :into forms)
     client)))


(defun get-tail (string forms &optional (end (length string)))
  "Given a list of forms, extract any trailing characters that were ignored."
  (let* ((tail (alexandria:lastcar forms))
         (tail-end (if tail
                       (cdr (node-source tail))
                       0)))
    (when (plusp (- end tail-end))
      (make-instance 'skipped-node
                     :content (subseq-displaced string tail-end end)
                     :source (cons tail-end end)))))

(defun post-process-nodes! (string forms start-at end-at)
  "Update each nodes in FORMS to include their prefix (extracted from STRING)."
  (let ((previous nil))
    (loop
      :for rest :on forms
      :for form :in forms
      :for lastp = (null (cdr rest))
      :for (form-start . form-end) = (node-source form)
      :for prefix-start = start-at then (cdr (node-source previous))

      :for end = (car (node-source form))
      :for prefix = (unless (zerop (- end prefix-start))
                      (subseq-displaced string prefix-start end))
      :do
         ;; update FORM
         (setf
          ;; Add the prefix
          (node-prefix form) prefix)
         ;; recurse
         (unless (terminalp form)
           (post-process-nodes! string (node-content form)
                                (car (node-source form))
                                (cdr (node-source form))))
         (when (and lastp (/= form-end end-at))
           (setf (cdr rest)
                 (cons
                  (make-instance 'skipped-node
                                 :content (subseq-displaced
                                           string form-end end-at)
                                 :source (cons form-end end-at))
                  nil)))
         ;; update loop variables
         (setf previous form))))


(defun parse-string (string)
  "Read STRING entirely using breeze's reader."
  ;; (read-all-forms string)
  ;; #+ (or)
  (let ((forms (read-all-forms string)))
    (if forms
        (progn
          (post-process-nodes! string forms 0 (length string))
          forms)
        (get-tail string forms))))


;;; "Unparsing"

;; Don't I have something similar in breeze.syntax-tree?
(defun write-raw (stream node)
  (write-string (node-raw node) stream))

(defun unparse-to-stream (stream nodes)
  "Print a list of NODES into STREAM."
  (let ((*print-case* :downcase)
        (*print-circle* t))
    (unparse-to-stream% stream nodes)))

(defun unparse-to-stream% (stream nodes)
  "Print a list of NODES into STREAM (implementation)."
  (dolist (node nodes)
    (alexandria:if-let
        ((prefix (node-prefix node)))
      (write-string prefix stream))
    (unparse-node stream node)))


(defgeneric unparse-node (stream node)
  (:documentation "Print a NODE into STREAM.")
  ;; Generic node
  (:method (stream (node node))
    (let ((content (node-content node)))
      (cond
        ((not (terminalp node))
         (unparse-to-stream% stream (node-content node))
         #+nil (when (listp content)
                 (write-char #\) stream)))
        ((typep content 'character-node)
         (princ (node-raw node) stream))
        (
         (format stream "~a" content)))))
  ;; Symbol
  (:method (stream (node symbol-node))
    (write-raw stream node))
  ;; String
  (:method (stream (node string-node))
    (write-raw stream node))
  ;; #+-
  (:method (stream (node feature-expression-node))
    ;; (unparse-node stream (node-feature-expression node))
    ;; (unparse-node stream (node-content node))
    (unparse-to-stream stream (node-content node)))
  ;; (...)
  (:method (stream (node list-node))
    (cond
      ((null (node-content node))
       (write-raw stream node))
      (t
       (unparse-to-stream% stream (node-content node))))))

(defun unparse-to-string (nodes)
  "Print a list of NODES as a STRING."
  (with-output-to-string
      (stream)
    (etypecase nodes
      (list
       (unparse-to-stream stream nodes))
      (node
       (unparse-to-stream stream (list nodes))))))


#+ (or)
(sb-profile:profile
 parse
 read-all-forms
 eclector.parse:read-from-string
 make-instance
 eclector.parse-result:make-expression-result
 eclector.parse-result:read-preserving-whitespace
 post-process-nodes!
 raw)

;; #+ (or) (sb-profile:report)

;; (log:config :debug)
