;; For examples of equivalent forms, search for the string "==" in
;; ~/quicklisp/dists/quicklisp/software/clhs-0.6.3/HyperSpec-7-0/HyperSpec/

;; definitely don't need the "if"
;; probably don't need the "null"
(if (null x)
    ()
  t)
=> x

;; the conditional are not needed
(if x
    x
  nil)
;; equivalent
(and x)
;; should be
x

;; The format is missing the stream parameter
(format "~a" 42)
;; should be
(format stream "~a" 32)
;; where STREAM is probably T or NIL

;; Too many forms in if (probably missing a progn)
(if p a b c)

;; no enough forms
(when)

;; trivial formats can be interchanged with specific functions
(format stream "~s" x) <=> (prin1 x stream)
;; there are many more instances of that (e.g. princ, write, etc)

;; the "when" is unessessary, it can be replaced by "and"
(when (and x y) y)
;; should
(and x y)

;; wrong convention
a_symbol
aSymbol
;; should be
a-symbol

;; Spacing is weird
(+(- 3 2)4)
;; should be
(+ (- 3 2) 4)

;; the indentation is wrong
(if
    pain
    tylenol
  water
  )
;; should be
(if pain
    tylenol
  water)
;; or
(if pain tylenol water)

;; TODO
(defun _ (&ptional))
makeunbound
make-unbound

extract-function
inline-function
#+nil use #+(or) instead

(case x
  (1 t)
  (3 t)
  (4 t)
  (t nil))
;; should be
(member x '(1 2 3) :test #'=)

(eql nil x)
;; same with equal or equalp, but not eq

(typep 'x nil)
;; you probably want
(typep 'x 'null)
;; same with check-type, declarations, types in struct and classes, etc.

keywordp

(typep x some-type-that-has-an-existing-predictate)
;; use the predicate instead
symbol keyword integer number string cons atom list

(typep 0 '(integer 1))
(plusp 0)


;; No need to nest variadic functions or macros
(and (and a b) c) => (and a b c)
(or (or a b) c) => (or a b c)
(+ (+ a b) c) => (+ a b c)
(* (* a b) c) => (* a b c)
(min (min a b) c) => (min a b c)
(max (max a b) c) => (max a b c)



;; nested car and cdr can be replaced by c[ad]+r functions
(car (cdr (cdr x))) <=> (caddr x)

;; bunch of cdr wrapped with a car can be replaced with nth (up to
;; tenth) or equivalent
(car (cdr (cdr x))) <=> (caddr x) <=> (third x) <=> (nth 2 x)
<=> (elt x 2)

;; first/rest == car/cdr
(first x) <=> (car x)
(rest x) <=> (cdr x)

;; rplaca rplacd can be replaced by setf
(rplaca x 1) <=> (setf (car x) 1)

;; can be swap dolist and certain loop
(loop :for x :in list :do ...)
(dolist (x list) ...)

;; You might want to replace a flet with labels
(flet ...) <=> (labels ...)

;; there's a function for that
(lambda (x) x)
;; should be
#'identity

;; there's a function for that
(lambda (_) 1)
;; should be
(constantly 1)


;; Can use alexandria's curry
(lambda (x)
  (fn 42 x))
;; can be replaced by
(curry 'fn 42)

;;; it can be useful too to replace curry, #'identity, constantly, etc
;;; by an actual lambda

;; extract a lambda into a defun
(lambda (x)
  (fn 42 x))
;; can be transformed into
(defun $name (x)
  (fn 42 x))
;; and at the call-site
#'$name



;; sometimes it can be useful to switch between 'find{,-if,-if-not}
;; and 'position{,-if,-if-not}
(find ...) <=> (position ...)
(find-if ...) <=> (position-if  ...)
(find-if-not ...) <=> (position-if-not  ...)

;; Inversions
t => nil
(find-if-not p ...) => (find-if p ...)
(position-if-not p ...) => (position-if p ...)
(remove-if-not p ...) => (remove-if p ...)
(if p a b) => (if (not p) a b) <=> (if p b a)
(when ...) => (unless ...)


;;
(quote ...) <=> '(...)
(function fn) <=> #'fn
#(...) => (make-array ...)


;; There are a bunch of forms you might want to wrap other forms with
eval-when
let
let*
flet
labels
destructuring-bind
multiple-value-list
multiple-value-bind
unwind-protect
locally
the
prog1
prog2
progn
progv
prog*


;; You might want to convert literal lists
(:a 1 :b 2) <=> ((:a . 1) (:b 2)) <=> ((:a 1) (:b 2))


;; TODO Convert defstruct <=> defclass (when possible)

;; Given a (defvar ) or (defparameter ), you might want to define a
;; bunch of accessors on it
(defvar *g* '((:a 1) (:b 3)))

(defun g (key)
  (assoc key *g*))

(defun (setf g) (key new-value)
  (setf (assoc key *g*) new-value))




(loop :for x :in '(nil t 1)
      :collect
      (loop :for y :in '(nil t 2)
            :collect
            (list
             (or x y)
             (when (or x y) y))))


;; In the loop clause
;; :for i :from 0 :below 10
;; the ":from 0" is unecesary



;; Don't mutate a literal
(rplaca '(1 . 2) 'issh)

;; Infinite recursion
(defun boom (list)
  (boom list))

;; The docstring can be put anywhere amongst the declarations
(defun foo (_)
  (declare (ignore _))
  "a docstring"
  (declare (ignore _)))

;; Look for duplicated code...


(x - y)
;; Probably meant
(- x y)

(defun is-bla ())
;; should be
(defun blap ())
;; or
(defun bla-p ())


(trace 'fun)
;; should be
(trace fun)




(let ((x y))
  (setf place x)
  x)
;; is equivalent to just
(setf place y)


;; I lost a bunch of time because I made a typo in the (:export ) of a
;; defpackage.


(if (listp x) x (coerce x))
;; is equivalent to
(coerce x)


(and ... t)
;; the "t" is useless
(or ... nil)
;; the "nil" is useless


;; When you have
(deftype nullable (typename) `(or null ,typename))
;; Then you can transform
(or null something)
;; Or
(or something null)
;; To
(nullable something)


;; Common mistake:
(or nil something)
;; instead of
(or null something)




(incf x y) <=> (decf x (- y))
;; when y < 0


(atom object) ==  (typep object 'atom) ==  (not (consp object))
==  (not (typep object 'cons)) ==  (typep object '(not cons))


(not (null x)) == (and x t)

;; it's redundent to add a quote before t, nil, or any keywords
':ok 'nil 'y

Bad:
(defmethod ((x '(:eql y))))
(defmethod ((x (:eql y))))
(defmethod ((x (:eql 'y))))
(defmethod ((x '(:eql 'y))))
(defmethod ((x 'y)))
(defmethod ((x :y)))
Good:
(defmethod (x (eql 'y)))
(defmethod (x (eql :y)))



(defun f (y)
  ;; Copy-pasted code, where the variables doesn't match, breeze
  ;; should be able to "quickfix" this relpacing either x or y by the
  ;; other.
  (if (plusp x)
      x
      (- x)))


;; Would be nice if we could detect this kind of typos...
(defu nasdf (...) ...)


Very bad:
char\=

Good:
char/=


(assoc "some string" alist)
=>
(assoc "some string" alist :test 'equal)


(return-from 'x)
=>
(return-from x)


(cons x nil) === (list x)


(not (= ...))
=>
(/= ...)


;; check for unused import-from and import

;; nested defun
;; defvar, defparameterm, defconstant inside defun
;; warn about non-toplevel defparameter, defvar, defconstant, defmacro ?
;; defparameter or defvar without earmuffs
;; defconstant without +...+



(let ((b (f1 a)))
  (and b (let ((c (f2 b)))
           (and c (f3 c)))))

=>

(let ((b (f1 a))
      (c (and b (f2 b))))
  (and c (f3 c)))

=>

(when-let* ((b (f1 a))
            (c (and b (f2 b))))
  (f3 c))



;; Warn that the body is empty
(let ((x 32)))
;; Same with flet, macrolet, symbol-macrolet, etc


let* (<only-one-binding>) => let


#|
Here's some cases that it might be possible to fix
automatically (like the typos):

1. odd number of &KEY arguments
might be hard to figure out which :key is missing or extraneous

2. Execution of a form compiled with errors.
Compile-time error:
  illegal function call

((a)) ; missing quote
((k . v))  ; missing quote

3. dot context error
((k . v . c))

4. More than one object follows . in list.
(x . y z
|#

;; we could "easily" fix these mistakes:
(loop :for i :below 2 :collec i)
#|
in sbcl:
unknown LOOP keyword: :COLLEC
current LOOP context: :COLLEC I.
[Condition of type SB-INT:SIMPLE-PROGRAM-ERROR]
|#


(with-output-to-string (*standard-input*) ...)
;; should be "-output*"
