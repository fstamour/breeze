(defpackage #:breeze.indirection
  (:documentation "Utilities for Inversion of Control without requiring changes to the
whole call stack.")
  (:use #:cl)
  (:export
   #:indirection ; a condition and a "catch tag"
   ;;; macros
   #:indirect
   #:with-indirections
   #:with-simple-indirections))

(in-package #:breeze.indirection)

(define-condition indirection ()
  ((form :initarg :form :reader form)))

(defmacro indirect (form)
  "Call FORM, but not directly, give a chance to override the evaluation
of FORM, the arguments of FORM are _always_ evaluated, except if the
car of FORM is cl:quote."
  (let* ((funcallp (and (listp form) (not (eq 'quote (car form)))))
         (args-var (gensym "args"))
         (quoted-form (if funcallp
                          ``(,',(car form) ,@,args-var)
                          form))
         (direct-form (if funcallp
                          `(apply ',(car form) ,args-var)
                          form))
         (body `(catch 'indirection
                  (signal 'indirection :form ,quoted-form)
                  ,direct-form)))
    (if funcallp `(let ((,args-var (list ,@(cdr form)))) ,body) body)))

#|
(macroexpand-1 '(indirect 42))
(CATCH 'INDIRECTION (SIGNAL 'INDIRECTION :FORM 42) 42)

(macroexpand-1 '(indirect x))
(CATCH 'INDIRECTION (SIGNAL 'INDIRECTION :FORM X) X)

(macroexpand-1 '(indirect 'x))
(CATCH 'INDIRECTION (SIGNAL 'INDIRECTION :FORM 'X) 'X)

(macroexpand-1 '(indirect (probe-file (format nil "~a.txt" 42))))
(LET ((#:|args19671| (LIST (FORMAT NIL "~a.txt" 42))))
  (CATCH 'INDIRECTION
    (SIGNAL 'INDIRECTION :FORM `(,'PROBE-FILE ,@#:|args19671|))
    (APPLY 'PROBE-FILE #:|args19671|)))


(indirect (probe-file (format nil "~a.txt" 42)))
(indirect (format nil "~a.txt" 42))
(indirect 42)

|#

(defvar *dispatchers* ())

#++
(progn
  (pop *dispatchers*)

  (push (lambda (form)
          (when (eq form 'x)
            (lambda ()
              '(indirect x))))
        *dispatchers*)

  (push (lambda (form)
          (when (eq form 'y)
            (lambda ()
              '(indirect y))))
        *dispatchers*)

  (setf *dispatchers* nil))

(defun find-handler (form)
  (loop :for dispatcher :in *dispatchers*
        :for handler = (funcall dispatcher form)
        :when handler :return handler))

(defun call-with-indirections (thunk)
  (handler-bind
      ((indirection
         (lambda (indirection)
           (let ((handler (find-handler (form indirection))))
             (throw 'indirection (if handler
                                     (funcall handler)
                                     (form indirection)))))))
    (funcall thunk)))

(defmacro with-indirections ((&body dispatchers) &body body)
  `(let ((*dispatchers* *dispatchers*))
     ,@(loop :for (var cond . replacement-forms) :in (reverse dispatchers)
             ;; This would be a good place to use something like
             ;; trivia's match...
             :collect `(push (lambda (,var)
                               (when ,cond
                                 (lambda ()
                                   ,@replacement-forms)))
                             *dispatchers*))
     (call-with-indirections
      (lambda ()
        ,@body))))

#++
(macrolet ((with-indirections ((&body dispatchers) &body body)
             `(let ((*dispatchers* *dispatchers*))
                ,@(loop :for (var cond . replacement-forms) :in (reverse dispatchers)
                        :collect `(push (lambda (,var)
                                         (when ,cond
                                           (lambda ()
                                             ,@replacement-forms)))
                                       *dispatchers*))
                (call-with-indirections
                 (lambda () ,@body)))))
  (with-indirections
      (((eq form 'x) '(indirect x))
       ((eq form 'y) '(indirect y)))
    (list
     (signal-indirection 'x)
     (indirect 'x)
     (let ((x 42))
       (indirect x))
     (indirect 'y)
     (indirect 'z))))

#++
(defun check-something (x)
  (indirect (probe-file "this file doesn't exits but this will return T because probe-file won't get called at all.")))

#++
(with-indirections
    (((eq (first form) 'probe-file) '+t+))
  (check-something 42))


(defmacro with-simple-indirections ((&body dispatchers) &body body)
  `(let ((*dispatchers* *dispatchers*))
     ,@(loop
         :with form = (gensym "form")
         :for (sym . replacement-forms) :in (reverse dispatchers)
         :collect `(push (lambda (,form)
                           (when (and (listp ,form)
                                      (eq ',sym (car ,form)))
                             (lambda ()
                               ,@replacement-forms)))
                         *dispatchers*))
     (call-with-indirections
      (lambda ()
        ,@body))))

#++
(indirect (+ 2 5))
;; => 7

#++
(with-simple-indirections
    ((+ 0))
  (indirect (+ 2 5)))
;; => 0
