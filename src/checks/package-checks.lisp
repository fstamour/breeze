(defpackage #:breeze.package-checks
  (:documentation "Package-related checks.")
  (:use #:cl)
  (:import-from #:breeze.checks
                #:defcheck)
  (:import-from #:breeze.package
                #:in-package-node-p)
  (:import-from #:breeze.parser
                #:node-string-designator))

(in-package #:breeze.package-checks)

;; TODO detect #:package:symbol => package:symbol
;; TODO warn when using double colon for external symbols (e.g. cl::defun)

;; TODO warn about using non-exported symbols token-node-p && length
;; package-marker == 2

#++
(defun check-in-package ()
  "Make sure the previous in-package form desginates a package that can
be found. If it's not the case (e.g. because the user forgot to define
a package and/or evaluate the form that defines the package) then show
a message and stop the current command."
  (let (($package (current-package-node (current-buffer))))
    (if $package
        (let ((package-name (node-string-designator $package)))
          (unless (find-package package-name)
            (message "The nearest in-package form designates a package that doesn't exists: ~s"
                     package-name)
            (return-from-command)))
        (;; TODO message no (in-package ...) found...
         (return-from-command)))))

(defcheck warn-undefined-in-package (node-iterator)
  :top-level-p t
  :livep t
  (alexandria:when-let* ((package-designator-node (in-package-node-p node-iterator))
                         (package-name (node-string-designator
                                        package-designator-node)))
    (unless (find-package package-name)
      (breeze.diagnostics:node-style-warning
       node-iterator
       (format nil "Package ~s is not currently defined." package-name)))))
