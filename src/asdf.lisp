;; https://asdf.common-lisp.dev/asdf.html
;; https://quickdocs.org/asdf-dependency-graph

(uiop:define-package #:breeze.asdf
    (:documentation "Utilities for adsf")
  (:nicknames #:basdf)
  (:use :cl #:alexandria)
  (:export
   #:system-files
   #:clear-fasl
   #:reload-system
   #:recompile-system
   #:system-directory
   #:loadedp))

(in-package #:breeze.asdf)

(defun system-files (system-designator &key (include-asd t))
  "List all the files in a system. Including the .asd file itself."
  (let ((system (asdf/system:find-system system-designator)))
    `(,@(when include-asd
          (list (asdf/system:system-source-file system)))
      ,@(remove-if #'uiop/pathname:directory-pathname-p
                   (mapcar #'asdf/component:component-pathname
                           (asdf/component:sub-components system))))))

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

#++
(defun apropos-system ())

;; TODO this report emacs and vim's temporary files
;; TODO this could take a predicate (to respect user preferences)
(defun find-files-that-should-be-added-to-a-system (system-designator &optional (file-type "lisp"))
  (flet ((shorten (root pathname-list)
           (mapcar #'(lambda (pathname)
                       (enough-namestring pathname root))
                   pathname-list)))
    (let* ((system (asdf/system:find-system system-designator))
           (root (asdf/system:system-source-directory system))
           (already-included (shorten root (system-files system-designator)))
           (directories (remove-if-not #'uiop/pathname:directory-pathname-p
                                       (mapcar #'asdf/component:component-pathname
                                               (asdf/component:sub-components system)))))
      (loop :for directory :in directories
            :append
            (remove-if #'(lambda (pathname)
                           (member pathname already-included
                                   :test #'equal))
                       (shorten
                        root
                        (directory
                         (make-pathname :name :wild
                                        :type file-type
                                        :directory (pathname-directory directory)))))))))

#++
(let ((system-designator 'breeze))
  (load (asdf/system:system-source-file system-designator))
  (find-files-that-should-be-added-to-a-system system-designator))


(defun find-sibling-systems (system-designator
                             &aux
                               (root (asdf:find-system system-designator))
                               siblings)
  (let ((directory (asdf:system-source-directory root)))
    (asdf:map-systems
     #'(lambda (system)
         (when (and (not (eq root system))
                    (equal directory
                           (asdf:system-source-directory system)))
           (push system siblings))))
    siblings))

#++
(find-sibling-systems "breeze")
;; => (#<ASDF/SYSTEM:SYSTEM "breeze/test"> #<ASDF/SYSTEM:SYSTEM "breeze/config">)

#++
(asdf:component-loaded-p
 (asdf:find-system "breeze"))

(defun infer-systems (pathname &aux systems)
  "Given a path (e.g. to a file), infer which systems it might be part of."
  (let* ((directory (uiop:pathname-directory-pathname pathname))
         ;; Try to find system definition files that aren't loaded.
         (asd-files (breeze.utils:find-asdf-in-parent-directories directory)))
    (asdf:map-systems
     #'(lambda (system
                &aux (dir (asdf:system-source-directory system)))
         (when (and dir
                    (or (uiop:subpathp directory dir)
                        (equal directory dir)))
           ;; Remove system definition files that are loaded.
           (setf asd-files (remove (asdf:system-source-file system)
                                   asd-files :test #'equal))
           (push system systems))))
    (append systems asd-files)))


#++
(infer-systems (truename "./"))

(defun loadedp (pathname &aux (pathname (uiop:truename* pathname)))
  "Check whether PATHNAME is part of a system, and wheter it was loaded.
This will return false if the file was loaded outside of asdf."
  (when pathname
    (loop :for system :in (infer-systems pathname)
          :for components = (asdf/component:sub-components system)
          :when (typep system 'asdf:system)
            :do (when-let* ((component-found (member
                                              pathname
                                              components
                                              :test #'equal
                                              :key #'asdf/component:component-pathname))
                            (component (first component-found)))
                  (return (values
                           (if (asdf:component-loaded-p component)
                               :loaded
                               :not-loaded)
                           (asdf:component-system component)))))))


#++
(loaded-p (breeze.utils:breeze-relative-pathname "src/asdf.lisp"))


;;; Inspecting a system's (transitive) dependencies

(defun system-dependencies (system-designator)
  (let ((system (asdf/system:find-system system-designator nil)))
    (remove-if-not #'stringp (asdf:system-depends-on system))))

(defun write-dependecy-graph (root-system &optional (stream t)
                              &aux
                                (system-ids (make-hash-table :test 'equal))
                                (sequential-id 0)
                                graph)
  ;; Walk the dependencies
  (labels ((deps (system-designator)
             (unless (gethash system-designator system-ids)
               (setf (gethash system-designator system-ids) (incf sequential-id))
               (loop
                 :with deps = (system-dependencies system-designator)
                 :for dep :in deps
                 :do (push (cons system-designator dep) graph)
                 :finally (mapcar #'deps deps)))))
    (deps root-system))
  ;; Output the graph
  (format stream "~&digraph {~%node [colorscheme=oranges9]")
  (loop :for system :being :the :hash-key :of system-ids :using (hash-value id)
        :do (format stream "~&node~d [label=\"~a\", color=~d, style=\"bold\"]"
                    id system
                    ;; the "orange9" colorscheme goes from 1 to 9,
                    ;; inclusively
                    (min 9 (1+ (length (system-dependencies system))))))
  (loop :for (from . to) :in graph
        :for from-id := (gethash from system-ids)
        :for to-id := (gethash to system-ids)
        :do (format stream "~&node~d -> node~d" from-id to-id))
  (format stream "~&}~%"))

#++
(alexandria:with-output-to-file (output
                                 (breeze.utils:breeze-relative-pathname "breeze.dot")
                                 :if-exists :supersede)
  (write-dependecy-graph "breeze" output))
;; dot -Tsvg breeze.dot > breeze.svg
