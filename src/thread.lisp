(uiop:define-package #:breeze.thread
    (:documentation "Utilities to help with concurrent programming.")
  (:use #:cl)
  (:import-from #:breeze.xref
                #:function-designator-p)
  (:export
   #:find-threads
   #:find-threads-by-name
   #:find-worker-threads
   #:kill-threads
   #:kill-threads-by-name
   #:kill-worker-threads
   #:breeze-kill-worker-threads))

(in-package #:breeze.thread)


;;; Thread management

(defun find-threads (&optional predicate (exclude-self-p t))
  (let ((current-thread (when exclude-self-p (bt:current-thread))))
    (remove-if-not #'(lambda (thread)
                       (and (not (eq current-thread thread))
                            (if predicate
                                (funcall predicate thread)
                                t)))
                   (bt:all-threads))))

(defun find-threads-by-prefix (prefix &key (exclude-self-p t))
  (find-threads #'(lambda (thread)
                    (alexandria:starts-with-subseq prefix (bt:thread-name thread)))
                exclude-self-p))

(defun find-threads-by-name (name &key (exclude-self-p t))
  (find-threads #'(lambda (thread)
                    (string= (bt:thread-name thread) name))
                exclude-self-p))

(defun find-worker-threads (&optional (exclude-self-p t))
  (find-threads-by-name "worker" :exclude-self-p nil))

(defun %kill-threads (threads)
  (prog1
      ;; Always return the number of threads
      (length threads)
    (when threads
      (mapcar #'bordeaux-threads:destroy-thread threads)
      (format *debug-io* "~&Killed ~d threads.~%" (length threads)))))

(defun kill-threads (predicate)
  "Find threads by predicate, and destroy them."
  (%kill-threads (find-threads predicate)))

(defun kill-threads-by-name (name)
  "Find threads by name, then destroy them."
  (%kill-threads (find-threads-by-name name)))

;; TODO make a command with this...
(defun kill-worker-threads ()
  "Find threads named \"worker\", then destroy them."
  (%kill-threads (find-threads-by-name "worker")))

(breeze.command:define-command breeze-kill-worker-threads ()
 "Find threads named \"worker\", then destroy them."
  (kill-worker-threads))
