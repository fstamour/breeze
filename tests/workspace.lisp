(defpackage #:breeze.test.workspace
  (:documentation "Test package for #:breeze.workspace")
  (:use #:cl #:breeze.workspace)
  (:import-from #:breeze.indirection
                #:with-simple-indirections)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish)
  (:export #:*breeze-workspace*))

(in-package #:breeze.test.workspace)



(define-test infer-project-name
  (false
   (with-simple-indirections
       ((breeze.utils:find-version-control-root))
     (infer-project-name "some path"))
   "infer-project-name should return nil if the version control root directory was not found")
  (is string= "foobar"
      (with-simple-indirections
          ((breeze.utils:find-version-control-root
            #p"/home/nobody/projects/foobar/"))
        (infer-project-name "some path"))
      "infer-project-name should return the name of the version control root directory when it is found"))

(defun goto-all-positions ($node)
  (loop
    :with length = (length (breeze.parser:source $node))
    :for i :below length
      :do (breeze.parser:goto-position $node i)))

(defparameter *breeze-workspace* nil)

;; this tests add-to-workspace
(define-test+run *breeze-workspace*
  (let ((*workspace* (make-instance 'workspace)))
    ;; TODO extract a function to "add system" into a workspace.
    (loop
      :with root = (breeze.utils:breeze-relative-pathname "") ; TODO use breeze.asdf:system-enough-pathname
      :for file :in (breeze.asdf:system-files 'breeze)
      :for name = (enough-namestring file root) ; TODO use breeze.asdf:system-enough-pathname
      :for content = (alexandria:read-file-into-string file)
      :do (add-to-workspace `(:buffer-name ,name
                              :buffer-string ,content
                              :point 1)))
    (setf *breeze-workspace* *workspace*)
    *workspace*))

(define-test+run add-to-workspace
  :depends-on (*breeze-workspace*)
  (finish
   (map-workpace-buffers
    (lambda (buffer
             &aux ($node (breeze.parser:node-iterator buffer)))
      (finish
           (progn ;; time
             (goto-all-positions $node))
           "Should be able to \"goto\" every positions in ~s" (name buffer))))))

(define-test+run locate-package-definition
  :depends-on (*breeze-workspace*)
  (let ((*workspace* *breeze-workspace*))
    (let ((location (finish (locate-package-definition "breeze.string"))))
      (true location
            "Should have been able to find the breeze.string's package definition.")
      (true (getf location :buffer)
            "Should have returned a plist that contains the key :buffer.")
      (true (getf location :node-iterator)
            "Should have returned a plist that contains the key :node-iterator.")
      (destructuring-bind (&key buffer node-iterator) location
        (when (and buffer node-iterator)
          (is string= "src/string-utils.lisp" (name buffer))
          (is string= #1="(defpackage #:breeze.string
  (:documentation \"String manipulation utilities\")"
              (subseq (breeze.parser:node-string node-iterator)
                      0 (length #1#))))))
    (let ((location (finish (locate-package-definition "breeze.parser"))))
      (true location
            "Should have been able to find the breeze.parser's package definition.")
      (true (getf location :buffer)
            "Should have returned a plist that contains the key :buffer.")
      (true (getf location :node-iterator)
            "Should have returned a plist that contains the key :node-iterator.")
      (destructuring-bind (&key buffer node-iterator) location
        (when (and buffer node-iterator)
          (is string= "src/parser.lisp" (name buffer))
          (is string= #2="(uiop:define-package #:breeze.parser
    (:documentation \"A fast, lossless, robust and "
              (subseq (breeze.parser:node-string node-iterator)
                      0 (length #2#))))))))
