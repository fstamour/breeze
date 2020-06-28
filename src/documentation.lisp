(in-package #:common-lisp-user)

(defpackage #:breeze.documentation
  (:nicknames ":br.doc")
  (:use :cl #:alexandria)
  (:export
   #:find-undocumented-symbols))

(in-package #:breeze.documentation)

(defun generic-method-p (symbol)
  "Returns T if SYMBOL designates a generic method"
  (subtypep
   (type-of (symbol-function symbol))
   'standard-generic-function))

(defun find-undocumented-symbols (package-designator)
  "Find symbols in a package that lacks documentation."
  (let ((result)
	(package (find-package package-designator)))
    ;; Package
    (unless (documentation package t)
      (push package result))
    (do-external-symbols (symbol package result)
      ;; Functions
      (when (and (fboundp symbol)
		 (not (documentation symbol 'function)))
	(if (generic-method-p symbol)
	    (loop :for method :in (closer-mop:generic-function-methods
				   (symbol-function symbol))
	       :unless (documentation method t)
	       :do (push method result))
	    (push (symbol-function symbol) result)))
      ;; Compiler macros
      (when (and (compiler-macro-function symbol)
		 (not (documentation symbol 'compiler-macro)))
	(push (compiler-macro-function symbol) result))
      ;; Variable
      (when (and (boundp symbol)
		 (not (documentation symbol 'variable)))
	(push symbol result))
      ;; TODO Method combination (documentation symbol 'method-combination)
      ;; TODO Classes and Structures (documentation symbol 'structure)
      ;; TODO Type specifiers (documentation symbol 'type)
      )))

;; To complete the 3 TODOs, I need to figure out:
;; how to check if a symbol represent a structure (for a class, I just need to use (find-class)
;; how to check if a symbol represent a type-specifier
;; hot to check if a symbol represent a method-combination
