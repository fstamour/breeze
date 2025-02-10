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
                #:finish))

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

(define-test add-to-workspace
  (finish
   (time
    (let ((*workspace* (make-instance 'workspace)))
      (loop
        :with root = (breeze.utils:breeze-relative-pathname "")
        :for file :in (breeze.asdf:system-files 'breeze)
        :for name = (enough-namestring file root)
        :for content = (alexandria:read-file-into-string file)
        :for buffer = (add-to-workspace `( :buffer-name ,name
                                           :buffer-string ,content
                                           :point 1))
        :for node-iterator = (parse-tree buffer)
        :do
           (breeze.logging:log-info "Going through ~s" name)
           (loop
             :with length = (length content)
             :for i :below length
             :for percent = (/ i length)
             :do
                ;; (breeze.logging:log-info "Going through ~s: ~D/~D" name i length)
                (breeze.lossless-reader:goto-position node-iterator i)))
      *workspace*))))
