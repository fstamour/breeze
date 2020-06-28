
(defpackage #:breeze.asdf
  (:documentation "Utilities for adsf")
  (:nicknames #:basdf)
  (:use :cl #:alexandria)
  (:export
   #:system-files
   #:clear-fasl
   #:reload-system
   #:recompile-system
   #:system-directory ))

(in-package #:breeze.asdf)

(defun system-files (system-designator)
  "List all the files in a system. Including the .asd file too."
  (let ((system (asdf/system:find-system system-designator)))
    `(,(asdf/system:system-source-file system)
      ,@(remove-if #'uiop/pathname:directory-pathname-p
		   (mapcar #'asdf/component:component-pathname
			   (asdf/component:sub-components
			    system))))))

(defun system-fasl-directory (system-designator)
  "Find the directory of a system's fasl files."
  (asdf:apply-output-translations
   (asdf:system-source-directory
    (asdf:find-system system-designator))))

(defun clear-fasl (system-designator)
  "Delete a system's fasl files."
  (uiop:delete-directory-tree
   (system-fasl-directory system-designator)
   :validate (constantly t)
   :if-does-not-exist :ignore))

(defun reload-system (system-designator)
  "Force to load a system again."
  (asdf:clear-system system-designator)
  (asdf:operate 'asdf:load-op system-designator))

(defun recompile-system (system-designator)
  "Useful to force recompiling a system after changing the *features*."
  (clear-fasl system-designator)
  (reload-system system-designator))

(defun system-directory (system-designator)
  "Get the system's directory."
  (uiop:pathname-directory-pathname
   (asdf:system-source-file
    (asdf:find-system system-designator))))

