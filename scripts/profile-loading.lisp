(cl:in-package #:cl-user)

(defpackage #:breeze.profile-loading
  (:documentation "A script to figure out which dependency takes the most time to load
when loading breeze.")
  (:use #:cl))

(in-package #:breeze.profile-loading)

(defvar *system-load-times* (make-hash-table :test 'eql))

;; Printing all the type of operations, to make sure I understand
;; asdf's operation order.
(defmethod asdf:perform :before ((op t) (system asdf:system))
  (format t "~&op: ~a system: ~a"  op system))

;; TODO We currently only check the time it takes to load a system,
;; not including the time it took to compile it.

(defmethod asdf:perform :before ((op asdf:load-op) (system asdf:system))
  (format t "op")
  (setf (gethash system *system-load-times*) (- (get-internal-run-time))))

(defmethod asdf:perform :after ((op asdf:load-op) (system asdf:system))
  (incf (gethash system *system-load-times*) (get-internal-run-time)))

(asdf:load-asd
 (merge-pathnames "../breeze.asd" *load-truename*))

;; This doesn't work if some packages where loaded from a read-only
;; store, like guix or nix...
(asdf:load-system "breeze" :force :all)


;; TODO Run this script in a container with sbcl and quicklisp set-up
;; TODO Analyze the results
