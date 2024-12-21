
(defpackage #:breeze.chanl-github-issue8
  (:documentation "")
  (:use #:cl))

(in-package #:breeze.chanl-github-issue8)

#|
2024-12-20 can't repro just yet, didn't try too much.

I did discover that if a thread is aborted while a debugger is opened
in slime, slime detects it and kill the debuger buffer.
|#

#|
 - create a thread that
   - creates another thread that kills the first one
   - after a small delay
 - (break)
|#

(bt:make-thread
 (lambda ()
   (let ((sacrifical-thread (bt:current-thread)))
     (bt:make-thread
      (lambda ()
        (sleep 1)
        (bt:destroy-thread sacrifical-thread))))
   (break)))


(bt:make-thread
 (lambda ()
   (let ((sacrifical-thread (bt:current-thread)))
     (bt:make-thread
      (lambda ()
        (sleep 1)
        (sb-thread:interrupt-thread
         sacrifical-thread
         (lambda ()
           (sb-thread:abort-thread))))))
   (break)))

(defparameter *sacrifical-thread*
  (bt:make-thread
   (lambda ()
     (break))))



(sb-thread:interrupt-thread
         *sacrifical-thread*
         (lambda ()
           (sb-thread:abort-thread)))
