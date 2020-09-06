(in-package #:common-lisp-user)

(defpackage #:breeze.documentation
  (:nicknames ":br.doc")
  (:documentation "Tools to inspect and generate documentation")
  (:use :cl #:alexandria)
  (:import-from #:breeze.xref
		#:generic-method-p
		#:specialp
		#:macrop
		#:classp
		#:simple-function-p
		)
  (:export
   #:find-undocumented-symbols))

(in-package #:breeze.documentation)

#|
    Classes and class slots
    Conditions
    Functions
    Generic functions
    Macros
    Methods
    Method combinations
    Packages
    Special variables and constants
    Structures
    Type definitions
|#


;;; Utilities

(defmacro map-external-symbol ((var package )
			       predicate-body
			       prelude-body
			       wrapper
			       &body loop-body)
  (once-only (package)
    (with-gensyms (symbols)
      `(spinneret:with-html
	 (let ((,symbols
		(sort
		 (loop :for ,var :being :the :external-symbol :of ,package
		    :when ,predicate-body
		    :collect ,var)
		 #'string<
		 :key #'symbol-name)))
	   (when ,symbols
	     ,prelude-body
	     ,(let ((loop `(loop :for ,var :in ,symbols
				,@loop-body)))
		(if wrapper
		    (list wrapper loop)
		    loop))))))))

#+nil
(map-external-symbol (sym (find-package :br))
    (boundp sym)
    (print "Symbol found")
    print
  :collect (print sym))

#+nil
(map-external-symbol
    (symbol (find-package :br))
    (boundp symbol)
    (:h3 "Special variables")
    :dl
  :do
  (:dt (symbol-name symbol))
  (:dd (documentation symbol 'variable)))



;;; Inspect documentation

(defun prettify-method (method)
  (let ((name (closer-mop:generic-function-name
	       (closer-mop:method-generic-function method)))
	(specializers (closer-mop:method-specializers method)))
    (list name specializers method)))

(defun find-undocumented-symbols (package-designator)
  "Find symbols in a package that lacks documentation."
  (let ((result)
	(package (find-package package-designator)))
    ;; Package
    (unless (documentation package t)
      (push (list :package package-designator) result))
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
	(push (list :special-variable symbol) result))
      ;; TODO Method combination (documentation symbol 'method-combination)
      ;; TODO Classes and Structures (documentation symbol 'structure)
      ;; TODO Type specifiers (documentation symbol 'type)
      )))

;; To complete the 3 TODOs, I need to figure out:
;; how to check if a symbol represent a structure (for a class, I just need to use (find-class)
;; how to check if a symbol represent a type-specifier
;; hot to check if a symbol represent a method-combination


;;; Generate documentation

(defun relative-pathname (pathname)
  (if (cl-fad:pathname-relative-p pathname)
      (asdf:system-relative-pathname :breeze pathname)
      pathname))

(defun render-markdown (pathname)
  "Read a markdown file and render it in spinneret's *html* stream."
  (let ((3bmd-tables:*tables* t))
    (3bmd:parse-and-print-to-stream
     (relative-pathname pathname)
     spinneret:*html*)))

;; TODO Move to utilities
(defun summarize (string)
  "Keep only the first sentence, remove parenthesis."
  (cl-ppcre:regex-replace-all
   "\\([^)]*\\) *"
   (if-let (position (position #\. string))
     (subseq string 0 position)
     string)
   ""))

(defun render-reference ()
  (spinneret:with-html
    (let ((packages
	   (sort
	    (breeze.xref:find-packages-by-regex "^breeze\\.[^.]+$")
	    #'string<
	    :key #'package-name)))
      #+nil
      (progn
	(:h1 "Packages' documentation")
	(loop
	   :for package :in packages
	   :for package-name = (string-downcase (package-name package))
	   :for docfile = (relative-pathname
			   (format nil "docs/~a.md" package-name))
	   :do
	     (if (probe-file docfile)
		 (progn
		   (:h2 (:a :name package-name package-name))
		   (render-markdown docfile))
		 (warn "Could not find \"~a\"." docfile))))
      (:h1 (:a :name "reference" "Reference"))
      ;; Package index
      (:dl
       (loop
	  :for package :in packages
	  :for package-name = (string-downcase (package-name package))
	  :do
	    (:dt (:a :href (format nil "#~A" package-name) package-name))
	    (:dd
	     (if-let (doc (documentation package t))
	       (summarize doc)))))
      (loop
	 :for package :in packages
	 :for package-name = (string-downcase (package-name package))
	 :for docfile = (relative-pathname
			 (format nil "docs/~a.md" package-name))
	 :do
	   (macrolet ((gen (title
			    predicate-body
			    documentation-type)
			`(map-external-symbol
			     (symbol package)
			     ,predicate-body
			     (:h3 ,title)
			     :dl
			   :do
			   (:dt (symbol-name symbol))
			   (:dd (or (documentation symbol
						   ,documentation-type)
				    "No documentation.")))))
	     (:h2 (:a :name package-name package-name))
	     (:p (or (documentation package t) "No description."))
	     (gen "Special variables" (specialp symbol) 'variable)
	     (gen "Classes" (classp symbol) 'type)
	     (gen "Generic methods" (generic-method-p symbol) 'function)
	     (gen "Functions" (simple-function-p symbol) 'function)
	     (gen "Macros" (macrop symbol) 'function))
	   ))))

(defun generate-documentation ()
  (let ((index (relative-pathname "docs/index.html"))
	(spinneret:*suppress-inserted-spaces* t)
	(spinneret:*html-style* :tree)
	(*print-pretty* nil))
    (with-output-to-file
	(spinneret:*html*
	 index
	 :if-exists :supersede
	 :if-does-not-exist :create)
      (spinneret:with-html
	(:doctype)
	(:html
	 (:head
	  (:title "Breeze")
	  (:link :rel "stylesheet" :href "style.css"))
	 (:body
	  (:ol
	   (:li (:a :href "#readme" "Breeze"))
	   (:li (:a :href "#emacs" "Emacs integration"))
	   (:li (:a :href "#reference" "Reference")))
	  (render-markdown "README.md")
	  (render-markdown "docs/emacs.md")
	  (render-reference)))))
    (format t "~%breeze.documentation: ~s written.~%" index)))
