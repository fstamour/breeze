(cl:in-package #:cl-user)

(require '#:asdf)

#-quicklisp
(let ((quicklisp-init #P"/opt/quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-asd
 (merge-pathnames "../breeze.asd" *load-truename*))

(flet ((find-all-related-systems (system)
         "Given a system, find all systems defined in the same system definition
file (including the one passed as argument)."
         (let ((result ())
               (asd-pathname (asdf:system-source-file system)))
           (asdf:map-systems (lambda (system)
                               ;; TODO Perhaps use asdf:primary-system-name
                               (when (equal asd-pathname
                                            (asdf:system-source-file system))
                                 (push system result))))
           result)))
  (let* ((systems (find-all-related-systems "breeze"))
         (dependencies (remove-if (lambda (system-name)
                                    (uiop:string-prefix-p "breeze" system-name))
                                  (loop
                                        :for system-name :in systems
                                        :for system = (asdf:find-system system-name)
                                        :for dependecy-list = (asdf:system-depends-on system)
                                        :append (copy-list dependecy-list)))))
    (ql:quickload dependencies)
    (mapcar #'asdf:register-immutable-system dependencies)))

(uiop:dump-image "dependencies.core")
