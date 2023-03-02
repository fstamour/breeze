
(uiop:define-package #:breeze.xref
    (:documentation "Cross-reference and introspection")
  (:mix :cl #:breeze.utils #:alexandria)
  (:export
   #:calls-who
   ;; Utilities
   #:find-packages-by-prefix
   #:find-packages-by-regex
   ;; Symbol inspection
   #:generic-method-p
   #:specialp
   #:macrop
   #:simple-function-p
   #:classp
   #:externalp))

(in-package #:breeze.xref)

(defun find-packages-by-prefix (prefix)
  "Find all packages whose name starts with the given prefix (case insensitive by default)."
  (loop
    :with prefix = (string-downcase prefix)
    :for package :in (list-all-packages)
    :when (starts-with-subseq prefix
                              (string-downcase
                               (package-name package)))
      :collect package))

(defun find-packages-by-regex (regex &optional (case-insensitive-p t))
  "Find all packages whose name match the regex (case insensitive by default)."
  (loop
    :with scanner = (cl-ppcre:create-scanner regex :case-insensitive-mode
                                             case-insensitive-p)
    :for package :in (list-all-packages)
    :when (cl-ppcre:scan scanner
                         (string-downcase
                          (package-name package)))
      :collect package))

(defun generic-method-p (symbol)
  "Returns T if SYMBOL designates a generic method"
  (and (fboundp symbol)
       (subtypep
        (type-of (fdefinition symbol))
        'standard-generic-function)))

(defun specialp (symbol)
  "Return true if SYMBOL is a special variable."
  (and (symbolp symbol)
       (or (boundp symbol)
           (eval `(let (,symbol)
                    (declare (ignorable ,symbol))
                    (boundp ',symbol))))))

(defun macrop (symbol)
  "Return true if SYMBOL designates a macro."
  (and (symbolp symbol)
       (macro-function symbol)))

(defun simple-function-p (symbol)
  "Return true if SYMBOL is a function that is nor a macro nor a generic function."
  (and (fboundp symbol)
       (not (generic-method-p symbol))
       (not (macrop symbol))))

(defun classp (symbol)
  "Return true if SYMBOL designate a class."
  (find-class symbol nil))

(defun externalp (symbol)
  (eq :external
      (nth-value 1 (find-symbol (symbol-name symbol)
                                (symbol-package symbol)))))
