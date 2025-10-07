(defpackage #:breeze.test.listener
  (:documentation "Tests for the package breeze.listener")
  (:use #:cl #:breeze.listener)
  (:import-from #:breeze.command
                #:*command*
                #:context
                #:current-buffer)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:breeze.test.command
                #:mock-send-out
                #:mock-recv-into
                #:with-fake-command-handler)
  (:import-from #:breeze.buffer
                #:make-buffer
                #:point
                #:update-buffer-content)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish
                #:fail))

(in-package #:breeze.test.listener)

;; TODO eval-last-expression
'asdf
'|asdf|
'|CL|::|IN-PACKAGE|
;; ^^^ Slime doesn't handle this one correctly

#++
breeze.dummy.test:hjkl

#+sbcl
(block nil
  (handler-bind
      ((sb-int:simple-reader-package-error
         (lambda (condition)
           (return condition))))
    (read-from-string
     "breeze.dummy.test:hjkl")))
;; => #<SB-INT:SIMPLE-READER-PACKAGE-ERROR "Symbol ~S not found in the ~A package." {10256B3563}>

#+sbcl
(block nil
    (handler-bind
        ((sb-int:simple-reader-package-error
           (lambda (condition)
             (return condition))))
      (read-from-string
       "breeze.dummy.dum.dum:hjkl")))
;; => #<SB-INT:SIMPLE-READER-PACKAGE-ERROR "Package ~A does not exist." {1025D737E3}>

(define-test+run interactive-eval-command ()
  ;; Empty file
  ;; TODO This should probably print a message instead...
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("pulse" 0 0) value)))
    (setf (gethash :buffer (context *command*))
          (make-buffer :string ""))
    (fail (interactive-eval-command)
        'end-of-file))
  ;; File with only 1 space
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("pulse" 0 1) value)))
    (setf (gethash :buffer (context *command*))
          (make-buffer :string " "))
    (fail (interactive-eval-command)
        'end-of-file))
  ;; TODO this test should work even if point == 0 or any upper value.
  (loop :for point :from 1 :upto 3 :do
    (with-fake-command-handler
        ((mock-send-out (value)
           (is equalp '("pulse" 1 3) value))
         (mock-send-out (value)
           (is equalp '("message" "BREEZE.TEST.LISTENER::X") value))
         (mock-send-out (value)
           (is equalp '("done") value)))
      ;; TODO make it less painful to setup a buffer correctly,,,
      (setf (gethash :buffer (context *command*))
            (make-buffer))
      (setf (point (current-buffer)) point)
      (update-buffer-content (current-buffer) " 'x ")
      (false (interactive-eval-command))))
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("pulse" 0 9) value))
       (mock-send-out (value)
         (is eqv '("message" :_) value)
         ;; TODO This is a hack... but it shows how the suggestions
         ;; are (for the moment) not deterministic.
         (let ((content (second value)))
           (cond
             ((string= content "Did you mean \"PRINT\"?")
              (is equalp '("message" "Did you mean \"PRINT\"?") value))
             ((string= content "Did you mean \"PRINC\"?")
              (is equalp '("message" "Did you mean \"PRINC\"?") value))
             ((string= content "Did you mean \"PRIN1\"?")
              (is equalp '("message" "Did you mean \"PRIN1\"?") value))
             (t
              (is equalp '("message" "Did you mean \"PRINT\"?") value)))))
       (mock-send-out (value)
         (is equalp '("message"
                      "32 (6 bits, #x20, #o40, #b100000)") value))
       (mock-send-out (value)
         (is equalp '("done") value))
       (mock-send-out (value)
         (is equalp '("done") value)))
    ;; TODO make it less painful to setup a buffer correctly,,,
    (setf (gethash :buffer (context *command*))
          (make-buffer))
    (setf (point (current-buffer)) 0)
    (update-buffer-content (current-buffer) "(prin 32)")
    ;; input "prin" candidate: PRINT
    (handler-bind
        ((undefined-function
           (lambda (condition)
             (declare (ignore condition))
             (let* ((restarts (compute-restarts))
                    (first-restart (car restarts)))
               (is eq 'breeze.listener::use-suggestion
                   (restart-name first-restart))
               (if (eq 'breeze.listener::use-suggestion
                       (restart-name first-restart))
                   (invoke-restart first-restart)
                   (error "Not the restart I expected: ~s" first-restart))))))
      (handler-bind
          ((style-warning (lambda (condition)
                            (muffle-warning condition))))
        (interactive-eval-command)))))
