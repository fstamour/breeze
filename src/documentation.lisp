(cl:in-package #:common-lisp-user)

(defpackage #:breeze.documentation
  (:documentation "Tools to inspect and generate documentation")
  (:use :cl #:alexandria)
  (:import-from #:breeze.xref
                #:generic-method-p
                #:specialp
                #:macrop
                #:classp
                #:simple-function-p)
  (:import-from #:breeze.html
                #:escape-html
                #:with-html-page)
  (:import-from #:breeze.string
                #:summarize
                #:with-fmt)
  (:export
   #:find-undocumented-symbols))

(in-package #:breeze.documentation)


;;; Inspect documentation

(defun prettify-method (method)
  (let ((name (closer-mop:generic-function-name
               (closer-mop:method-generic-function method)))
        (specializers (closer-mop:method-specializers method)))
    (list name specializers method)))

;; TODO
#|
- [ ] Classes
- [ ] Class slots
- [ ] Conditions
- [X] Functions
- [X] Generic functions
- [X] Macros
- [X] Methods
- [ ] Method combinations
- [X] Packages
- [X] Special variables
- [ ] constants
- [ ] Structures
- [ ] Type definitions (I'm not sure this one can be done with introspection alone).
|#
;; TODO this doesn't detect if the documentation string actually has anything (could be the empty string).
(defun find-undocumented-symbols (package-designator)
  "Find symbols in a package that lacks documentation."
  (let ((result)
        (package (find-package package-designator)))
    ;; Package
    (unless (documentation package t)
      (push (list :package (package-name package)) result))
    (do-external-symbols (symbol package result)
      ;; Functions
      (when (and (fboundp symbol)
                 (not (documentation symbol 'function)))
        ;; Generic functions
        (cond
          ((generic-method-p symbol)
           (let ((has-documented-methods-p nil))
             (loop :for method :in (closer-mop:generic-function-methods
                                    (symbol-function symbol))
                   :if (documentation method t)
                     :do (setf has-documented-methods-p t)
                   :else
                     :do (push (append '(:method) (prettify-method method)) result))
             (unless has-documented-methods-p
               (push (list :generic-method symbol) result))))
          ((macrop symbol)
           (push (list :macro symbol) result))
          (t
           (push (list :function symbol) result))))
      ;; Compiler macros
      (when (and (compiler-macro-function symbol)
                 (not (documentation symbol 'compiler-macro)))
        (push (list :compiler-macro symbol) result))
      ;; Variable
      (when (and (specialp symbol)
                 (not (documentation symbol 'variable)))
        (push (list :special-variable symbol) result)))))

;; To complete the 3 TODOs, I need to figure out:
;; how to check if a symbol represent a structure (for a class, I just need to use (find-class)
;; how to check if a symbol represent a type-specifier
;; how to check if a symbol represent a method-combination


;;; Utilities for document generation

(defmacro map-external-symbol ((var package
                                &key predicate
                                  before
                                  after)
                               &body loop-body)
  (once-only (package)
    (with-gensyms (symbols)
      `(let ((,symbols
               (sort
                (loop :for ,var :being :the :external-symbol :of ,package
                      :when (eq ,package (symbol-package ,var))
                        :when ,predicate
                          :collect ,var)
                #'string<
                :key #'symbol-name)))
         (when ,symbols
           ,before
           (loop :for ,var :in ,symbols
                 ,@loop-body)
           ,after)))))

;; TODO turn this into a test
#++
(map-external-symbol (sym (find-package :breeze.string)
                      :predicate (boundp sym)
                      :before (format t "~%Symbols found"))
  :do (print sym))


;; TODO turn this into a test
#++
(with-output-to-string (out)
  (with-fmt (out)
    (map-external-symbol
        (symbol (find-package :breeze.workspace)
         :predicate (boundp symbol)
         :before (fmt "<h3>Special variables</h3>~%"))
      :do
      (fmt "<dt>~a</dt>~%" (escape-html (symbol-name symbol)))
      (fmt "<dd>~a</dd>~%" (escape-html (documentation symbol 'variable))))))


;; TODO
(defun function-lambda-list (function)
  "Returns a function's lambda-list"
  (declare (ignorable function))
  nil)


;;; Generate documentation

;; TODO
#|
- [X] Classes
- [ ] Class slots
- [ ] Conditions
- [X] Functions
- [X] Generic functions
- [X] Macros
- [X] Methods
- [ ] Method combinations
- [X] Packages
- [X] Special variables
- [ ] constants
- [ ] Structures
- [ ] Type definitions (I'm not sure this one can be done with introspection alone).
|#

(defun render-reference (out packages)
  (with-fmt (out)
    (let ((packages (sort (copy-list packages)
                          #'string<
                          :key #'package-name)))
      (fmt "<h1><a id=\"reference\">Reference</a></h1>~%")
      ;; Package index
      (fmt "<dl>~%")
      (loop
        :for package :in packages
        :for package-name = (escape-html (string-downcase (package-name package)))
        :do
           (fmt "<dt><a href=\"#~a\">~a</a></dt>~%"
                package-name package-name)
           (fmt "<dd>~a</dd>~%"
                (escape-html
                 (if-let (doc (documentation package t))
                   (summarize doc)))))
      (fmt "</dl>~%")
      (loop
        :for package :in packages
        :for package-name = (escape-html (string-downcase (package-name package)))
        :do
           (macrolet ((gen (title
                            predicate
                            documentation-type)
                        `(map-external-symbol
                             (symbol package
                              :predicate ,predicate
                              :before (progn
                                        (fmt "<h3>~a</h3>" ,title)
                                        (fmt "<dl>"))
                              :after (fmt "</dl>"))
                           :do
                           (fmt "<dt>~a ~a</dt>"
                                (escape-html (symbol-name symbol))
                                (escape-html (function-lambda-list symbol)))
                           (fmt "<dd>~a</dd>"
                                (escape-html
                                 (or (documentation symbol
                                                    ,documentation-type)
                                     "No documentation."))))))
             (fmt "<h2><a id=\"~a\">~a</a></h2>" package-name package-name)
             (fmt "<p>~a</p>"
                  (escape-html (or (documentation package t) "No description.")))
             (gen "Special variables" (specialp symbol) 'variable)
             (gen "Classes" (classp symbol) 'type)
             (gen "Generic methods" (generic-method-p symbol) 'function)
             (gen "Functions" (simple-function-p symbol) 'function)
             (gen "Macros" (macrop symbol) 'function))))))

(defun generate-documentation-to-stream (stream packages)
  (with-html-page (stream)
    (fmt "<title>Reference</title>")
    (fmt "<body>")
    (render-reference stream packages)
    (fmt "</body>")))

(defun generate-documentation (root packages)
  (let* ((index (merge-pathnames "reference.html" root))
         #+(and sbcl windows)
         (sb-impl::*default-external-format* :utf-8))
    (ensure-directories-exist root)
    (with-output-to-file
        (output
         index
         :if-exists :supersede
         :if-does-not-exist :create)
      (generate-documentation-to-stream output packages)
      (format *trace-output* "~%breeze.documentation: ~s written.~%" index))))
