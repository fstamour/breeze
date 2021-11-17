;;; Trying to refactor stuff

(in-package #:common-lisp-user)

(defpackage #:refactor-scratch
  (:use :cl)
  (:import-from #:breeze.reader
		#:parse-string
		#:list-node
		#:symbol-node
		#:node-content)
  (:shadowing-import-from #:breeze.reader
			  #:read-from-string))

(in-package #:refactor-scratch)

(defun defpackage-node-p (node)
  (and
   (typep node 'list-node)
   (let ((car (car (node-content node))))
     (and (typep car 'symbol-node)
	  (string-equal "defpackage" (node-content car))))))

(matchp
 '(defpackage *)
 (make-instance 'list-node :content (make-instance 'symbol-node :content "defpackage")))

(read-from-string "(defpackage name)")
#<LIST-NODE (#<SYMBOL-NODE DEFPACKAGE> #<SYMBOL-NODE NAME>)>

(defclass defpackage-node (list-node)
  ())

(typep (make-instance 'defpackage-node) 'list-node)
;; => t

;; ===> I could have a ton of specialized classes to help with manipulating the syntax tree

(let ((node (read-from-string "(defpackage name)")))
  (and
   (typep node 'list-node)
   (let ((car (car (node-content node))))
     (and (typep car 'symbol-node)
	  (string-equal "defpackage" (node-content car))))))


 (parse-string "(defpackage name)")
