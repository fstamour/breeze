(in-package #:common-lisp-user)

(uiop:define-package #:breeze.dummy.test
    (:mix #| :breeze.definition |# :cl)
  (:nicknames :dum)
  (:export
   ;; Documented symbols
   #:*bound-variable*
   #:*unbound-variable*
   #:a-class
   #:a-function
   #:a-generic-function
   #:a-macro
   #:slot
   #:an-integer

   ;; Undocumented symbols
   #:*bound-variable-undocumented*
   #:*unbound-variable-undocumented*
   #:class-undocumented
   #:function-undocumented
   #:generic-function-undocumented
   #:macro-undocumented
   #:slot-undocumented
   #:integer-undocumented

   #:another-generic-function

   ;;
   #:mul
   #:2x
   #:add-one))

(in-package #:breeze.dummy.test)


;;; Documented symbols

(defvar *unbound-variable* ()
  "A documented unbound symbol.")

(defvar *bound-variable* t
  "A documented bound symbol.")

(defun a-function ()
  "A documented function."
  t)

(defgeneric a-generic-function ()
  (:documentation "A documented generic function."))

(defmethod a-generic-function ()
  "A method."
  t)

(defmacro a-macro ()
  "A documented macro."
  t)

(defclass a-class ()
  ((slot
    :accessor slot
    :documentation "A documented slot."))
  (:documentation "A documented class."))

(deftype an-integer () "A documented type-specifier." '(integer 0 100))


;;; Undocumented symbols

(defvar *unbound-variable-undocumented* ())
(defvar *bound-variable-undocumented* t)
(defun function-undocumented ())
(defgeneric generic-function-undocumented ())
(defmethod generic-function-undocumented ())
(defmacro macro-documented ())
(defclass class-undocumented ()
  ((slot-undocumented
    :accessor slot-undocumented)))

(deftype integer-undocumented () '(integer 0 100))


;;; Other cases

(defgeneric another-generic-function (x))
(defmethod another-generic-function ((x (eql '1))))
(defmethod another-generic-function ((x (eql '2)))
  "documented" t)


;;;

(defun mul (x y)
  "Multiply x by y."
  (* x y))

(defun 2x (x)
  "Multiply x by 2."
  (mul 2 x))

(defun add-one (x)
  "Add 1 to x."
  (1+ x))

#+ (or)
(deftest mul
  (is (= 4 (mul 2 2)))
  (is (= 12 (mul 2 6))))

#+ (or)
(deftest 2x
  (is (= (2x 2) (mul 2 2))))

#+ (or)
(deftest should-fail
  (is (= 6 (mul 2 2))))
