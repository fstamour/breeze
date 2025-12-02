;; https://github.com/ksluckow/awesome-symbolic-execution
;; https://en.wikipedia.org/wiki/Symbolic_execution

#|

use cases:
- inferring a function's type
- inferring a macro's type
  -end goal: smart completion, maybe suggest adding `check-type' s to the macro

- "propagate" a default value of a function, see if it's equivalent to another
- propagate the "actual used values"...
  - if a function supports many types, but is always called with the same value or type.

|#

(defpackage #:breeze.abstract-symbolic
  (:documentation "Abstract interpretation and symbolic execution.")
  (:use #:cl #:breeze.generics))

(in-package #:breeze.abstract-symbolic)


#|

Ok, as an exploratory phase, I'm just going to try to walk some forms
and see what is needed to extract any useful information.

For now, I'll just accumulate everything (in context's trail).

|#


;; TODO *env*
;; TODO pass context everywhere
;; context ~= environment + constraints...

(defclass context ()
  ((counter
    :initform -1
    :initarg :counter
    :accessor counter
    :documentation "Counter for numbering temporary variables")
   (trail
    :initform nil
    :initarg :trail
    :accessor trail
    :documentation "The trail of information found while walking the forms."))
  (:documentation "The context™"))

(defclass item ()
  ((predecessors
    :initform nil
    :initarg :predecessors
    :accessor predecessors
    :documentation ""))
  (:documentation "An item of a trail."))

(defclass variable-binding (item named)
  ((var
    :initform nil
    :initarg :var
    :accessor var
    :documentation "Name of the generated temp var.")
   (initial-value
    :initform nil
    :initarg :initial-value
    :accessor initial-value
    :documentation "Initial value of the binding."))
  (:documentation "Denotes the creation of a new binding."))

(defmethod print-object ((binding variable-binding) stream)
  (let ((*print-case* :downcase))
    (format stream "(~s ~s ~s ~s)"
            (name (class-of binding))
            (var binding)
            (name binding)
            (initial-value binding))))

(defclass lexical-binding (variable-binding) ()
  (:documentation "Denotes the creation of a new lexical-binding."))

(defclass dynamic-binding (variable-binding) ()
  (:documentation "Denotes the creation of a new dynamic-binding."))

(defun variable-binding-p (x)
  (typep x 'variable-binding))



(defclass unbound (item named) ()
  (:documentation "Denotes the reference to a variable that doesn't have a corresponding
variable binding."))

(defmethod unbound ((symbol symbol))
  (make-instance 'unbound :name symbol))

(defmethod print-object ((unbound unbound) stream)
  (let ((*print-case* :downcase))
    (flet ((pp (x)
             (cond
               ((keywordp x) (prin1-to-string x))
               ((symbolp x) (format nil "'~s" x))
               (t (prin1-to-string x)))))
      (format stream "(unbound ~a)" (pp (name unbound))))))


(defun unboundp (x)
  (typep x 'unbound))



;; TODO test (print-to-string (unbound 'x))

(defun note (context thing)
  "Push THING on top of CONTEXT's trail."
  (push thing (trail context))
  thing)

(defun temp (context)
  "Generate a new temp variable symbol"
  (alexandria:format-symbol *package* "T-~d" (incf (counter context))))

(let ((c (make-instance 'context)))
  (temp c))
;; => T-0


(defun careq (l x)
  (and (consp l)
       (eq (car l) x)))

(defun find-binding (context symbol)
  (loop :for item :in (trail context)
        :when (variable-binding-p item)
          :do (with-slots (var name initial-value) item
                (declare (ignore initial-value))
                (when (eq symbol name)
                  ;; (return var)
                  (return item)))
        :finally
           (let ((unbound (unbound symbol)))
             (note context unbound)
             (return unbound))))



(defgeneric walk (form)
  (:documentation "Walk FROM and gather information about it in a `context'
object. Returns the context."))

(defgeneric %walk (context form)
  (:documentation "The implementation of `walk'."))

(defgeneric walk-list (context car cdr)
  (:documentation "Dispatch on car."))



;; entry point:
(defmethod walk (form)
  (let ((context (make-instance 'context)))
    (%walk context form)
    ;; TODO I only return the trail as a second value to make it
    ;; easier to develop
    (values context (trail context))))


(defmethod %walk (context form)
  (etypecase form
    (null (note context :null))
    (list (walk-list context (car form) (cdr form)))
    (atom
     (cond
       ((symbolp form)
        (let ((binding (find-binding context form)))
          (if (variable-binding-p binding)
              (var binding)
              binding)))
       ((constantp form nil)
        (note context (list :constant form))
        form)
       (t (error "Don't knpw how to handle ~s" form))))))

(defmethod walk-list (context car cdr)
  ;; TODO if we know the type of the arguments of the function, we
  ;; should "propagate" the types to the arguments (cdr).
  (note context
        ;; TODO create class for variable assignment
        (list := (temp context)
              (list
               :funcall car
               (loop :for form :in cdr
                     :collect (%walk context form))))))

(defmethod walk-list (context (car (eql 'if)) cdr)
  (destructuring-bind (test-form then-form &optional else-form)
      cdr
    (note context
          (list :join
                (%walk context test-form)
                (%walk context then-form)
                (%walk context else-form)))))

(defmethod walk-list (context (car (eql 'let)) cdr)
  (destructuring-bind (bindings &rest body)
      cdr
    (let ((declarations (loop
                          :for form :in body
                          :while (careq form 'declare)
                          :collect form))
          (forms (member-if (lambda (form) (not (careq form 'declare)))
                            body)))
      ;; process bindings
      (dolist (binding bindings)
        (let* ((var (if (listp binding) (car binding) binding))
               (init (when (listp binding) (second binding)))
               (temp (temp context))
               (init (%walk context init)))
          (note context
                ;; TODO (lexical-binding temp var (%walk context init))
                (make-instance
                 'lexical-binding
                 :var temp
                 :name var #|confusing...|#
                 :initial-value init
                 :predecessors init))
          (when (constantp init nil)
            (note context (list :constant temp)))))
      ;; process declarations
      (loop :for (_declare . decl) :in declarations
            :do (note context (list :declare decl)))
      ;; process bpdy (implicit progn)
      (loop :for form :in forms
            :for x := (%walk context form)
            ;; return the last form
            :finally (return x)))))

(trace walk %walk walk-list)

(walk '(* x 2))
;; =>
;; #<CONTEXT {10139E83C3}>
;; ((:= T-0 (:FUNCALL * (X 2))) (:UNBOUND X))

(walk '(if p 1 2))
;; =>
;; #<CONTEXT {1013B10413}>
;; ((:= T-0 (:FUNCALL IF (P 1 2))) (:UNBOUND P))

(walk '(if p 1))
;; =>
;; #<CONTEXT {1013C783C3}>
;; ((:= T-0 (:FUNCALL IF (P 1))) (:UNBOUND P))

(walk '(let ((x 2))
        (declare (type number x))
        (* x 2)))
#|
#<CONTEXT {10202C83C3}>
((:= T-1 (:FUNCALL * (T-0 2)))
 (:DECLARE ((TYPE NUMBER X)))
 (:CONSTANT T-0)
 (:BIND T-0 X 2))
|#

(walk '(let ((x 2))
        (let ((y (* x 3))))
        (* y 2)))

(walk '(let ((x 2))
        (let (y
              (* x 3))
          (* y 2))))

(walk '(let ((x 2))
        (let ((y (* x 3)))
          (* y 2))))

(defmethod interpret ((context context) env)
  (let ((env (or env (make-instance 'context))))
    (loop :for item :in (reverse (trail context))
          :for value := (interpret item env)
          :finally (return value)
          ;; :when value
            ;; :collect value
            )))

(defmethod interpret ((item list) env)
  (case (car item)
    (:constant item)
    (:= (destructuring-bind (var form)
            (cdr item)
          (let ((value (interpret form env)))
            (note env (make-instance 'lexical-binding :var var)
                  ;; (list var value)
                  )
            value)))
    (:funcall
     (destructuring-bind (fn args)
         (cdr item)
       (let ((interpreted-args (mapcar (lambda (x)
                                         (interpret x env))
                                       args)))
         ;; TODO test if it is safe to funcall fn and if all the
         ;; interpreted args are "not abstract"/valid.
         (if (some (alexandria:disjoin
                    'null
                    'unboundp)
                   interpreted-args)
             (list :funcall fn interpreted-args)
             (apply #'funcall fn interpreted-args)))))))

(defmethod interpret (x env)
  (cond
    ((constantp x nil) x)
    ((symbolp x) (find-binding env x))))

(interpret 1 nil)

(interpret (walk 1) nil)
;; => (:constant 1)

;; a really contrived way to compute 2 + 2
(interpret (walk '(+ 1 1)) nil)
;; => 2

(interpret (walk '(+ 1 x)) nil)
;; => (:FUNCALL + (1 (unbound 'x)))


(interpret (walk '(let ((x 2))
                   (+ 1 x)))
           nil)
;; because it's missing a "find-binding"
(:FUNCALL + (1 NIL))
