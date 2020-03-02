
(defpackage #:breeze.utils
  (:use :cl)
  (:export
   #:package-apropos
   #:walk
   #:walk-car
   #:walk-list
   #:with-collector
   #:with-values
   #:collect))

(in-package #:breeze.utils)

(defun walk (tree fn  &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on every elements"
  (dolist (node tree)
    (if (listp node)
        (when (funcall recurse-p)
          (walk node fn recurse-p))
        (funcall fn node))))

(defun walk-list (tree fn &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on each list parts"
  (when (listp tree)
    (funcall fn tree)
    (dolist (node tree)
      (when (funcall recurse-p node)
        (walk-list node fn recurse-p)))))

(defun walk-car (tree fn &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on each first elements (cars)"
  (walk-list tree
             #'(lambda (node)
                 (funcall fn (car node)))
             recurse-p))

(defmacro with-collector (&body body)
  (let ((collector (gensym "collector")))
    `(let ((,collector ()))
       (macrolet ((collect (value) `(push ,value ,',collector)))
         ,@body)
       (nreverse ,collector))))


(defmacro with-values (values &body body)
  `(let ,values ,@body
     (values ,@(mapcar #'first values))))

(defun package-apropos (search-string)
  "Compute a list of package that contains the search-string."
  (remove-if-not #'(lambda (package)
                     (search search-string (package-name package)
                             :test #'string-equal))
                 (list-all-packages)))


