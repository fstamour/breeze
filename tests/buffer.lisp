
(defpackage #:breeze.test.buffer
  (:documentation "Test package for #:breeze.buffer")
  (:use #:cl #:breeze.buffer)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.buffer)

(define-test+run base
  (of-type 'buffer (make-buffer))
  (is string= "#<buffer nil>"
           (prin1-to-string (make-buffer)))
  (is string= "#<buffer \"foo.lisp\">"
           (prin1-to-string (make-buffer :name "foo.lisp")))
  (is string= "#<buffer \"foo.lisp\">"
           (prin1-to-string (make-buffer :name "foo.lisp"
                                         :string ""))))

(define-test+run current-package
  (false (let* ((string "(in-package #:cl-user)")
                (buffer (make-buffer :string string)))
           (setf (point buffer) 0)
           (current-package buffer)))
  (false (let* ((string "(in-package #:cl-user)")
                (buffer (make-buffer :string string)))
           (setf (point buffer) 10)
           (current-package buffer)))
  (is equalp '(:uninterned "CL-USER")
      (let* ((string "(in-package #:cl-user)")
                (buffer (make-buffer :string string)))
           (setf (point buffer) (length string))
           (let (($node (current-package buffer)))
             (breeze.analysis:parse-symbol-node $node)))))
