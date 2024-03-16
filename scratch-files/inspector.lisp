;;; Exploration: trying to generate a "place expression" from the
;;; current state of slime's inspector.
;;;
;;; Idea from @Akasha on discord:
;;; https://discord.com/channels/297478281278652417/297478350145060875/1183809496087277663
#|

@Akasha wanted something to generate a form when opening the inspector
from the debugger, but I'll start simpler: inspecting a very simple
expression, like a symbol...

|#

(in-package :cl-user)

(require 'alexandria)

(eval-when (:load-toplevel :execute)
  (setq *print-circle* t))

;; Let's create a dummy value to inspect
(defparameter *x* (alexandria:plist-hash-table
                   `(:a "asdf"
                     :b (a (nested (list #(with-an-array 42))))
                     :c ,(cons :x :y)
                     ;; :circular #1=(x . #1#)
                     :nested ,(alexandria:plist-hash-table
                               `(
                                 1 "un"
                                 2 "deux"
                                 3 "trois"))
                     :improper-list ,(list* 'a 'b 'c)
                     )))



#+ elisp
(progn
  (define-key lisp-mode-map (kbd "M-c") #'eval-defun)
  (define-key lisp-mode-map (kbd "C-c C-f")
    (lambda ()
      (interactive)
      (let ((slime-buffer-package "CL-USER"))
        (slime-inspect "*x*")))))


(in-package :swank)

;;; Inspecting the inspector's state

#++
(progn
  (istate.object *istate*)
  ;; => #<HASH-TABLE :TEST EQL :COUNT 2 {1012F082D3}>


  (istate.content *istate*)
  #|
  (
  "Count" ": " (:VALUE 2) (:NEWLINE)
  "Size" ": " (:VALUE 7) (:NEWLINE) "Test"
  ": " (:VALUE EQL) (:NEWLINE)
  "Rehash size" ": " (:VALUE 1.5) (:NEWLINE)
  "Rehash threshold" ": " (:VALUE 1.0) (:NEWLINE)
  (:ACTION "[clear hashtable]"
  #<FUNCTION (LAMBDA () :IN EMACS-INSPECT) {10152E821B}>)

  (:NEWLINE) "Contents: " (:NEWLINE)

  (:VALUE :A) " = " (:VALUE "asdf") " "
  (:ACTION "[remove entry]"
  #<FUNCTION (LAMBDA () :IN EMACS-INSPECT) {10152E823B}>) (:NEWLINE)

  (:VALUE :B) " = " (:VALUE (A (NESTED (LIST #(WITH-AN-ARRAY 42)))))
  " "
  (:ACTION "[remove entry]"
  #<FUNCTION (LAMBDA () :IN EMACS-INSPECT) {10152E825B}>)
  (:NEWLINE)
  )|#)

;;; Figuring out how the navigation into nested structures works

#|

In the inspectors' buffer RET is bound to
slime-inspector-operate-on-point, here's its docstring:

Invoke the command for the text at point.
1. If point is on a value then recursivly call the inspector on
that value.
2. If point is on an action then call that action.
3. If point is on a range-button fetch and insert the range.

Internally, it uses (slime-inspector-property-at-point) to know what's
under the point. In turns, this return the first property found
between slime-part-number, slime-range-button and slime-action-number.

slime-inspector-operate-on-point calls swank:inspect-nth-part to
inspect other values

|#

#++
(progn
  (istate.parts *istate*)
  #(
    ;; #<HASH-TABLE :TEST EQL :COUNT 2 {10152C8003}>
    2
    7
    EQL
    1.5
    1.0
    :A
    "asdf"
    :B
    (A (NESTED (LIST #(WITH-AN-ARRAY 42))))))

#++
(trace
 swank:init-inspector
 swank::reset-inspector
 swank:inspector-eval
 swank:inspect-nth-part
 swank::inspect-object
 swank:emacs-inspect
 swank:inspector-reinspect)


;;; First of all: we lose the original form in init-inspector,
;;; inspector-eval, inspect-in-frame (which uses eval-in-frame)

(defparameter *place* nil)

;; changes: save the initial form into *place*
(defslimefun init-inspector (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME inspection request.")
      (reset-inspector)
      (let ((form (read-from-string string)))
        (setf *place* (list form))
        (inspect-object (eval form))))))



;;; Now, when swank:inspect-nth-part is called, how do we know if the
;;; part is a place "inside" the object being inpected? Because there
;;; are many "parts" which are just metadata of the object (like its
;;; type, length, etc.)

;; changes: add place; save a cons into PARTS instead of OBJECT
(defun value-part (object string place parts)
  (list :value
        (or string (print-part-to-string object))
        (assign-index (cons object place)
                      parts)))

;; changes: add &optional place
(defun iline (label value &optional place)
  `(:line ,label ,value ,place))

;; changes: add &key place
(defun label-value-line (label value &key (newline t) place)
  "Create a control list which prints \"LABEL: VALUE\" in the inspector.
If NEWLINE is non-NIL a `(:newline)' is added to the result."
  (list* (princ-to-string label) ": " `(:value ,value nil ,place)
         (if newline '((:newline)) nil)))

;; changes: add &optional place
(defmacro label-value-line* (&rest label-values)
  ` (append ,@(loop for label-value in label-values
                    collect (destructuring-bind (label value &optional place)
                                label-value
                              `(label-value-line ,label ,value :place ,place)))))

;; changes: add &optional place to :value and non-optional to :line
(defun prepare-part (part istate)
  (let ((newline '#.(string #\newline)))
    (etypecase part
      (string (list part))
      (cons (dcase part
              ((:newline) (list newline))
              ;; added "place"
              ((:value obj &optional str place)
               (list (value-part obj str place (istate.parts istate))))
              ((:label &rest strs)
               (list (list :label (apply #'cat (mapcar #'string strs)))))
              ((:action label lambda &key (refreshp t))
               (list (action-part label lambda refreshp
                                  (istate.actions istate))))
              ((:line label value place)
               (list (princ-to-string label) ": "
                     (value-part value nil place (istate.parts istate))
                     newline)))))))

;; these migth not be up-to-date
#++
(flet ((test-prepare-part (object part)
         (let* ((istate (make-istate :object object :previous nil
                                     :verbose *inspector-verbose*)))
           (prepare-part part istate))))
  (values
   (test-prepare-part 'x `(:value x))
   (test-prepare-part 'x `(:value x "X"))
   (test-prepare-part 'x `(:value x nil (gethash *)))))

;; changes: handle cons instead of objects directly
(defslimefun inspect-nth-part (index)
  (with-buffer-syntax ()
    (destructuring-bind (object . place)
        (inspector-nth-part index)
      (push place *place*)
      (inspect-object object))))

;; changes: keep *place* in sync
(defslimefun inspector-pop ()
  "Inspect the previous object.
Return nil if there's no previous object."
  (with-buffer-syntax ()
    (cond ((istate.previous *istate*)
           (pop *place*)
           (setq *istate* (istate.previous *istate*))
           (istate>elisp *istate*))
          (t nil))))



(defun place ()
  (car *place*))

;; new around method: prepend the "place"
(defmethod emacs-inspect :around (o)
  (let ((parts (call-next-method))
        (line (iline "Place" (place))))
    (if (listp parts)
        `(,line ,@parts)
        (lcons line parts))))



;; changes: add "car" and "cons" places
(defun inspect-cons (cons)
  (label-value-line*
   ('car (car cons) `(car ,(place)))
   ('cdr (cdr cons) `(cdr ,(place)))))

(defun inspect-list-aux (list)
  (loop for i from 0
        for rest on list
        while (consp rest)
        append
        (if (listp (cdr rest))
            (label-value-line i
                              (car rest)
                              :place `(nth ,i ,(place)))
            (label-value-line* (i (car rest))
                               (:tail (cdr rest) `(cdr (last ,(place))))))))


;; changes: add places to values
(defmethod emacs-inspect ((ht hash-table))
  (append
   (label-value-line*
    ("Count" (hash-table-count ht))
    ("Size" (hash-table-size ht))
    ("Test" (hash-table-test ht))
    ("Rehash size" (hash-table-rehash-size ht))
    ("Rehash threshold" (hash-table-rehash-threshold ht)))
   (let ((weakness (hash-table-weakness ht)))
     (when weakness
       (label-value-line "Weakness:" weakness)))
   (unless (zerop (hash-table-count ht))
     `((:action "[clear hashtable]"
                ,(lambda () (clrhash ht))) (:newline)
       "Contents: " (:newline)))
   (let ((content (hash-table-to-alist ht)))
     (cond ((every (lambda (x) (typep (first x) '(or string symbol))) content)
            (setf content (sort content 'string< :key #'first)))
           ((every (lambda (x) (typep (first x) 'real)) content)
            (setf content (sort content '< :key #'first))))
     (loop for (key . value) in content appending
           `((:value ,key) " = " (:value ,value nil
                                         ;; Added this:
                                         (gethash ,key ,(place)))
             " " (:action "[remove entry]"
                          ,(let ((key key))
                             (lambda () (remhash key ht))))
             (:newline))))))

;; changes: add "row-major-aref" place
(defun emacs-inspect-array-aux (array)
  (unless (= 0 (array-total-size array))
    (lcons*
     "Contents:" '(:newline)
     (labels ((k (i max)
                (cond ((= i max) '())
                      (t (lcons (iline i (row-major-aref array i) `(row-major-aref ,(place) ,i))
                                (k (1+ i) max))))))
       (k 0 (array-total-size array))))))

;; changes: add "row-major-aref" place
(defun emacs-inspect-vector-with-fill-pointer-aux (array)
  (let ((active-elements? (< 0 (fill-pointer array)))
        (inactive-elements? (< (fill-pointer array)
                               (array-total-size array))))
    (labels ((k (i max cont)
               (cond ((= i max) (funcall cont))
                     (t (lcons (iline i (row-major-aref array i) `(row-major-aref ,(place) ,i))
                               (k (1+ i) max cont)))))
             (collect-active ()
               (if active-elements?
                   (lcons*
                    "Active elements:" '(:newline)
                    (k 0 (fill-pointer array)
                       (lambda () (collect-inactive))))
                   (collect-inactive)))
             (collect-inactive ()
               (if inactive-elements?
                   (lcons*
                    "Inactive elements:" '(:newline)
                    (k (fill-pointer array)
                       (array-total-size array)
                       (constantly '())))
                   '())))
      (collect-active))))


;;; Extra notes

#|

When re-inspecting, the original *package* is lost, meaning that the
symbols that were not printed with a package qulifier might have one
after re-inspecting, even though we're inspecting the exact same
object(s).

It would be nice to be able to have multiple inspectors at the same
time.

It would be nice to easily re-evaluate the original form. Perhaps even
re-read it!

This feature would be better implemented either directly in swank, or
as it's own inspector (there is a "fancy inspector" contrib...)

This feature would go well with the "egraph refactoring" feature...

Because the inspector has "place" form, it could be easier to edit
stuff in the inspector...

|#
