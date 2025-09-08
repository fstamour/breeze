;;;; Utilities for <a href="https://asdf.common-lisp.dev/asdf.html">ASDF</a>

(uiop:define-package #:breeze.asdf
  (:documentation "Utilities for asdf")
  (:nicknames #:basdf)
  (:use :cl #:alexandria)
  (:export
   #:system-files
   #:find-all-related-systems
   #:find-all-related-files
   #:clear-fasl
   #:reload-system
   #:recompile-system
   #:system-directory
   #:loadedp))

(in-package #:breeze.asdf)

(defun find-all-related-systems (system)
  "Given a system, find all systems defined in the same system definition
file (including the one passed as argument)."
  (let ((result ())
        (asd-pathname (asdf:system-source-file system)))
    (asdf:map-systems (lambda (system)
                        ;; TODO Perhaps use asdf:primary-system-name
                        (when (equal asd-pathname
                                     (asdf:system-source-file system))
                          (push system result))))
    result))

(defun system-files (system-designator &key (include-asd t))
  "List all the files in a system. Including the .asd file itself."
  (let ((system (asdf/system:find-system system-designator)))
    `(,@(when include-asd
          (list (asdf/system:system-source-file system)))
      ,@(remove-if #'uiop/pathname:directory-pathname-p
                   (mapcar #'asdf/component:component-pathname
                           (asdf/component:sub-components system))))))

(defun find-all-related-files (system)
  "List all files in SYSTEM and in the other systems defined in the same
system definition file."
  (remove-duplicates
   (alexandria:flatten
    (mapcar #'system-files (find-all-related-systems system)))))

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

#|

TODO fix loadedp

Here's a complex bug that I never noticed:

1. The function loadedp calls infer-systems
2. loadedp will crash if it gets a path to a system file that doesn't
contain any loaded system
3. infer-systems returns only system definition files that contains a
system that has not been loaded

Which means that loadedp crashes when it's called on a file that is
part of a system that no systems from the same system definition file
was loaded.

It's subtle...

The fix is (and/or):
1. Test if I can get away with `(asdf:load-asd ...`
2. add an ignore-error or something similar, because loadedp should
never ever crash... But I would like some warning if an issues arise.
Perhaps add an &optional errorp
3. Maybe find an alternative to asdf/component:sub-component
4. List the systems in the system file and check if any of them are
loaded, if the file is not part of any of these, then the file is
likely not loaded

|#


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

(defun asd-file-registered-p (path-to-asd)
  ;; this maps over asdf/system-registry:*registered-systems*
  (etypecase path-to-asd
    (asdf:system path-to-asd)
    (pathname
     (asdf:map-systems
      (lambda (system)
        (when-let ((pathname (asdf/system:system-source-file system)))
          (when (equal pathname path-to-asd)
            (return-from asd-file-registered-p system))))))))

#++
(mapcar
 (lambda (x)
   (list (cons :designator x)
         (cons :was-registed-p (and (asd-file-registered-p x) t))
         (cons :system (if (pathnamep x) (list x (asdf:load-asd x)) x))))
 (infer-systems (truename "./")))


;; TODO if it's not part of a system, check if it should
(defun loadedp (pathname &aux (pathname (uiop:truename* pathname)))
  "Check whether PATHNAME is part of a system, and whether it was loaded.
This will return false if the file was loaded outside of asdf."
  ;; TODO This function should never fail, add a handler-case
  (ignore-errors ; 🤯
   (when pathname
     (loop :for system-or-pathname-to-asd :in (infer-systems pathname)
           :for system = (or
                          ;; Check if it's a system (and if it's in
                          ;; asdf/system-registry:*registered-systems*)
                          (asd-file-registered-p system-or-pathname-to-asd)
                          ;; TODO think about the implications of loading asd files on-the-go...
                          ;; 1. it might make it easier to work on stuff outside asdf' registry
                          ;; 2. it _will_ make it hard to work on different copies of the same project...
                          ;; 3. it will probably lead to confusing situation
                          ;; TODO actually ask permissions before doing this!
                          (asdf:load-asd system-or-pathname-to-asd)
                          ;; this will lookup the system in asdf/system-registry:*registered-systems*
                          (asd-file-registered-p system-or-pathname-to-asd))
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
                            (asdf:component-system component))))))))


#++
(loaded-p (breeze.utils:breeze-relative-pathname "src/asdf.lisp"))


;;; Inspecting a system's (transitive) dependencies
;;; See also <a href="https://quickdocs.org/asdf-dependency-graph">asdf-dependency-graph</a>

(defun system-dependencies (system-designator)
  (let ((system (asdf/system:find-system system-designator nil)))
    (remove-if-not #'stringp (asdf:system-depends-on system))))

;; TODO use https://github.com/gpcz/cl-uniquifier/ to generate the labels?

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
