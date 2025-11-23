
(defpackage #:breeze.invert
  (:documentation "Command to invert the form at point.")
  (:use #:cl))

(in-package #:breeze.invert)


(unless (donep $node)
  (cond
    ((match (sym :wild t) $node)
     ;; replace by nil
     )
    ((match (sym :wild nil) $node)
     ;; replace by t
     )))

;; TODO true <=> false
;; TODO if => swap then-form and else-form, or invert the condition
;; TODO < <=> >=
;; TODO > <=> <=
;; TODO = >= /=
;; TODO eq => (not eq)
;; TODO while => until
;; TODO when => unless (or invert the condition)
;; TODO (null x) <=> x
;; TODO {string,char} {=,-{,not-}equal,<,<=,>,>=,-{,not-}lessp,-{,not-}greaterp
;; TODO is <=> isnt (parachute)

string-
