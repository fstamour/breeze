(in-package #:common-lisp-user)

(defpackage #:cl-metadata
  (:use :cl :alexandria))

(in-package #:cl-metadata)

;; Get the list of "functions" in the common-lisp package.
(defparameter *functions*
  (let ((result ()))
    (do-symbols (sym (find-package :cl))
      (when (fboundp sym)
	(print sym)
	(push sym result)))
    result))

;; Sort them
(sort *functions*
      (lambda (s1 s2)
	(string< (symbol-name s1) (symbol-name s2))))

;; Get the list of special operators
(defparameter *special-operators* (remove-if-not #'special-operator-p *functions*))


;; Some stats
;; (length *functions*) => 730
;; (length *special-operators*) => 25



;;; Trying to categorize the list of functions
(defparameter *functionals* (make-hash-table))

;; (hash-table-count *functionals*)

;; TODO I would like to defer questions about symbols that starts with these prefixes:
;; '(bit- array- hash-table- symbol- prin char-)

;; Ask the user if a function is "functional"
(loop :for fn :in *functions* :do
      (multiple-value-bind (last-answer has-answer-p)
	  (gethash fn *functionals*)
	(declare (ignorable last-answer))
	(unless has-answer-p
	  (let ((answer nil))
	    (loop :while (not (member answer '(y n s q))) :do
		  (format t "Is ~A functional? (y/n/s/q (yes/no/skip/quit)  " fn)
		  (setf answer (read)))
	    (when (eq 'q answer)
	      (return))
	    (setf (gethash fn *functionals*) answer)))))

;;;; TODO Ask if a function is "mathematical"
;;;; Maybe we could infer some things?


(defun symbol-starts-with (prefix symbol)
  (starts-with-subseq prefix (symbol-name symbol)))

(defun function-that-starts-with (prefix)
  (remove-if-not (curry 'symbol-starts-with prefix) *functions*))



;; Dump the answers so far
(hash-table-alist *functionals*)



'((CHAR . S) (CHANGE-CLASS . N) (CERROR . S) (CELL-ERROR-NAME . Y) (CEILING . Y)
 (CDR . Y) (CDDR . Y) (CDDDR . Y) (CDDDDR . Y) (CDDDAR . Y) (CDDAR . Y)
 (CDDADR . Y) (CDDAAR . Y) (CDAR . Y) (CDADR . Y) (CDADDR . Y) (CDADAR . Y)
 (CDAAR . Y) (CDAADR . Y) (CDAAAR . Y) (CCASE . N) (CATCH . N) (CASE . Y)
 (CAR . Y) (CALL-METHOD . S) (CADR . Y) (CADDR . Y) (CADDDR . Y) (CADDAR . Y)
 (CADAR . Y) (CADADR . Y) (CADAAR . Y) (CAAR . Y) (CAADR . Y) (CAADDR . Y)
 (CAADAR . Y) (CAAAR . Y) (CAAADR . Y) (CAAAAR . Y) (BYTE-SIZE . Y)
 (BYTE-POSITION . Y) (BYTE . S) (BUTLAST . Y) (BROADCAST-STREAM-STREAMS . Y)
 (BREAK . N) (BOUNDP . Y) (BOTH-CASE-P . Y) (BOOLE . N) (BLOCK . S)
 (BIT-XOR . S) (BIT-VECTOR-P . S) (BIT-ORC2 . S) (BIT-ORC1 . S) (BIT-NOT . S)
 (BIT-NOR . S) (BIT-NAND . S) (BIT-IOR . S) (BIT-EQV . S) (BIT-ANDC2 . S)
 (BIT-ANDC1 . S) (BIT-AND . S) (BIT . S) (ATOM . N) (ATANH . Y) (ATAN . Y)
 (ASSOC-IF-NOT . Y) (ASSOC-IF . Y) (ASSOC . Y) (ASSERT . S) (ASINH . Y)
 (ASIN . Y) (ASH . S) (ARRAYP . Y) (ARRAY-TOTAL-SIZE . Y)
 (ARRAY-ROW-MAJOR-INDEX . Y) (ARRAY-RANK . Y) (ARRAY-IN-BOUNDS-P . Y)
 (ARRAY-HAS-FILL-POINTER-P . Y) (ARRAY-ELEMENT-TYPE . Y)
 (ARRAY-DISPLACEMENT . Y) (ARRAY-DIMENSIONS . Y) (ARRAY-DIMENSION . Y)
 (ARITHMETIC-ERROR-OPERATION . Y) (ARITHMETIC-ERROR-OPERANDS . Y) (AREF . Y)
 (APROPOS-LIST . Y) (APROPOS . Y) (APPLY . S) (APPEND . S) (AND . Y)
 (ALPHANUMERICP . Y) (ALPHA-CHAR-P . Y))



(defun set-functional (symbol answer)
  (setf (gethash symbol *functionals*) answer))

;; Correcting my mistakes
;; (set-functional 'ccase 'n)
;; (set-functional 'call-method 's)





;;;; If a function take a function (i.e. map and remove-if) is it functional.
;;;; Yes!

;;;; The byte functions return a newly allocated object (AFAIK) but it always returns equivalent
;;;; object given the same parameters. Is it functional???




;;; Trying to get the list of higher-order functions and which
;;; parameter is another function.
;;; In order to generate better quickfixes

(sb-introspect:function-arglist #'mapcar)

(loop :for function :in *functions*
      :for pos = (unless (macro-function function)
		   (position 'function
			     (sb-introspect:function-arglist function)))
      :when pos
	:collect (cons function pos))


((FUNCALL . 0) (MAP-INTO . 1) (MAPCON . 0) (MAPL . 0) (SET-PPRINT-DISPATCH . 1)
	       (MAPCAR . 0) (REDUCE . 0) (MAPLIST . 0) (COMPLEMENT . 0)
	       (SHARED-INITIALIZE . 17) (MAP . 1) (MAPCAN . 0) (SET-MACRO-CHARACTER . 1)
	       (MAPC . 0) (SET-DISPATCH-MACRO-CHARACTER . 2) (APPLY . 0))

;; apart from "shared-initizlize", it looks good


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(:import-from :breeze.xref
	      #:generic-method-p
	      #:specialp
	      #:macrop
	      #:simple-function-p
	      #:classp)



(defun lambda-list-keyword-p (symbol)
  (and (member symbol cl:lambda-list-keywords) t))

(defparameter *symbols*
  (flet ((pred (predicate symbol &optional name)
	   (when (funcall predicate symbol)
	     `(,(or name (alexandria:make-keyword predicate))))))
    (loop for symbol being the external-symbols of (find-package 'cl)
	  collect
	  `(,symbol ;; Does the symbol have function?
	    ,@(pred 'fboundp symbol)
	    ;; Is the symbol a lambda list keyword? (e.g. &optional, &key, etc.)
	    ,@(pred 'lambda-list-keyword-p symbol)
	    ;; Is the symbol a variable?
	    ,@(pred 'boundp symbol)
	    ;; Is the symbol a type-specifier
	    #+sbcl
	    ,@(pred 'sb-ext:valid-type-specifier-p symbol :type-specifier)
	    ;; Does the symbol represent a class?
	    ,@(pred 'classp symbol :class)))))


(setf (gethash "test" *cl-symbols*) :42)
(gethash "TEST" *cl-symbols*)
