
(defpackage #:breeze.package
  (:documentation "Package utilities")
  (:use #:cl #:breeze.analysis)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:export #:in-package-node-p
           ;; re-export from breeze.analysis
           #:map-top-level-in-package))

(in-package #:breeze.package)

;; TODO I want to check if a node is an "in-package" node...
;; - [ ] case converting if necessary
;; - [x] skip whitespaces
;; - [x] check if there's a package designator
;;
;; Now, I have a chicken-and-egg issue because of
;; package-local-nicknames...  I need to know what is the current
;; package to look for PLNs to find the in-pacakge form, but I need
;; the in-package to know the current package.

(define-node-matcher in-package-node-p ((in-package :?package-designator))
  (unless (quotedp node-iterator)
    (when-let* ((package-designator (breeze.analysis::get-bindings :?package-designator))
                (package-designator-node (value package-designator)))
      ;; TODO string-designator-node-p
      (when (or (token-node-p package-designator-node)
                (string-node-p package-designator-node)
                (sharp-uninterned-node-p package-designator-node))
        ;; TODO else... it's a malformed in-package form
        package-designator))))

;; TODO add tests
(defmethod map-top-level-in-package (function (state state))
  "Map FUNCTION over all top-level (in-package ...) forms in STATE."
  (map-top-level-forms
   (lambda (node-iterator)
     (when-let ((package-name (in-package-node-p node-iterator)))
       (funcall function node-iterator package-name)))
   state))

;; TODO add tests
(defmethod locate-package-definition ((package package) #| TODO haystack |#)
  (locate-package-definition (package-name package)))

;; TODO add tests
(defmethod locate-package-definition ((package node-iterator) #| TODO haystack |#)
  (locate-package-definition (node-string-designator-string package)))
