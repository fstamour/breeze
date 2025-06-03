(in-package #:breeze.workbench)

(defvar *default-trace-report-default* sb-debug:*trace-report-default*)

;; tracing is very very useful for debugging, but the default way sbcl
;; often prints "way too much" stuff
(defun trace-report (depth function event stack-frame values)
  (declare (ignorable stack-frame))
  ;; (pprint-logical-block stream values :prefix ... :suffix ...)
  (let ((*print-pretty* nil)
        (stream *standard-output*))
    (terpri stream)
    (pprint-logical-block (stream values
                                  :per-line-prefix (format nil "~v@{~A~:*~}" depth "  |"))
      ;; (loop :repeat depth :do (format stream "  |"))
      (pprint-indent :current depth stream)
      (case event
        (:enter
         (format stream "~3d (~a ~{~a~^ ~})" depth function values))
        (:exit
         (format stream "~3d => ~{~a~^, ~}" depth values))
        (t
         (format stream "~3d ~s (~a ~{~a~^ ~})" depth event function values))))))

(setf sb-debug:*trace-report-default* 'trace-report)
