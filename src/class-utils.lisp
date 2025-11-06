(defpackage #:breeze.class-utils
  (:documentation "")
  (:use #:cl #:breeze.generics)
  (:export #:eqv
           #:define-class))

(in-package #:breeze.class-utils)

(defvar *print-quote* t
  "Controls whether to print single quotes when printing symbols.")

;; TODO setup a print-table that uses *print-quote*, use that
;; print-table in the print-object method defined by define-class
;; see (defmethod print-object ((sym sym) stream) ...) in pattern.lisp

;; TODO this is looking more and more like defstruct, I think it would
;; be nice if the syntax was similar too.
;; e.g. (:constructor) instead of :constructor t
(defmacro define-class (name
                        (&key
                           superclass
                           positional-args
                           keyword-args
                           (predicate t)
                           (constructor t))
                        &body defclass-body)
  "Helper macro to define a class along with common functions.

Defines:
- a class
- a constructor function for the class
- a predicate function for testing if an object is an instance of the class
- a print-object method for the class
- an eqv method for comparing instances of the class


NAME will be used as the name of the class

POSITIONAL-ARGS and KEYWORD-ARGS are used for the constructor and the
print-object method.

CONSTRUCTOR can be set to nil to skip the constructor function.

DEFCLASS-BODY, as the name suggest, is used as the body (everything
after the list of superclasses) of a cl:defclass form.
"
  (let* ((slots (first defclass-body))
         (initargs
           ;; TODO keyword-args is assumed to be a list of
           ;; symbol, so default values are not supported yet
           (when keyword-args
             (loop :for kw :in keyword-args
                   :append (list (alexandria:make-keyword kw) kw))))
         ;; extract the initargs from the list of slot specifiers
         (initargs (append
                    initargs
                    (loop
                      :for slot :in slots
                      :for initarg := (when (listp slot)
                                        (getf (rest slot) :initarg))
                      :unless (keywordp initarg)
                        :do (warn "Initarg ~s was expected to be a keyword." initarg)
                      :when (and initarg
                                 (not (member initarg positional-args :test #'string=))
                                 (not (member initarg initargs)))
                        :append `(,initarg
                                  ,(intern (symbol-name initarg))))))
         (plist (loop :for s :in positional-args
                      :append (list (alexandria:make-keyword s) s))))
    `(progn
;;; class
       (defclass ,name (,@(when superclass (list superclass)))
         ,@(or (list slots)`(()))
         ,@(rest defclass-body))
;;; predicate
       ,(when predicate
          `(defun
               ;; name of the predicate
               ,(if (eq t predicate)
                    (alexandria:symbolicate name '-p)
                    predicate)
               ;; lambda list
               (node)
             ;; docstring
             ,(format nil "Is this a node of type ~s" name)
             ;; predicate's implementation
             (typep node ',name)))
;;; constructor
       ,(when constructor
          `(defun
               ;; name of the constructor function
               ,name
               ;; lambda list
               (,@positional-args
                &key ,@(remove-if #'keywordp initargs))
             ,(format nil "Make a node of type ~s" name)
             (let* ,(when (listp constructor)
                      (getf constructor :let*))
               ;; constructor's implementation
               (make-instance ',name ,@plist ,@initargs))))
;;; print-object
       (defmethod print-object ((node ,name) stream)
         (let ((*print-case* :downcase))
           (write-char #\( stream)
           ;; print the type of the node
           (write-string ,(let ((*print-case* :downcase))
                            (prin1-to-string name)) stream)
           ;; print the positional arguments
           ,@(loop :for arg :in positional-args
                   :collect `(format stream " ~s" (,arg node)))
           ;; print the keyword arguments. this assumes that they
           ;; all default to nil
           ,@(loop :for (kw arg) :on initargs
                   :by #'cddr
                   :collect `(let ((,arg (,arg node)))
                               (when ,arg
                                 (format stream " ~s ~s"
                                         ,kw ,arg))))
           (write-char #\) stream)))
;;; eqv
       (defmethod eqv ((a ,name) (b ,name))
         (or (eq a b)
             ,@(when (or positional-args initargs)
                 `((and
                    ;; compare the positional arguments
                    ,@(loop :for arg :in positional-args
                            :collect `(eqv (,arg a) (,arg b)))
                    ;; compare the keyword argumentsv
                    ,@(loop :for (kw arg) :on initargs
                            :by #'cddr
                            :collect `(eqv (,arg a) (,arg b)))))))))))

#++
(define-class a (:positional-args (start end)
                 :keyword-args (errors)
                 )
  ((errors :initarg :errors :accessor errors)))
