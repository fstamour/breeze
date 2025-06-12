(defpackage #:breeze.test.package-commands
  (:documentation "Tests for the package breeze.package-commands")
  (:use #:cl #:breeze.package-commands)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.package-commands)

;; TODO Variants: *insert-defpackage/cl-user-prefix*
;; TODO infer-project-name
;; TODO infer-is-test-file
;; TODO infer-package-name-from-file
#++
(define-test+run insert-defpackage
  (let* ((trace (drive-command #'insert-defpackage
                               :inputs '("pkg")
                               :context '())))

    (common-trace-asserts 'insert-defpackage trace 4)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Name of the package: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (second trace)
      (is string= "pkg" input)
      (is string= "insert" (first request))
      (is equal "(defpackage " (second request)))
    (destructuring-bind (input request) (third trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("#:pkg"
            "  (:documentation \"\")"
            "  (:use #:cl))"
            ""
            "(in-package #:pkg)")
          (split-by-newline (second request))))))
