;;;; See fndb.lisp and knownfn.lisp in <a href="https://github.com/sbcl/sbcl/blob/master/src/compiler/fndb.lisp">sbcl</a>
;;;; and <a href="https://github.com/phoe/portable-condition-system/blob/1307ec146d227a9d8ea42312c1ba2a5206a9eb3c/t/ansi-test-data.lisp">phoe/portable-condition-system</a>
;;;;
;;;;
;;;; See https://github.com/informatimago/lisp/blob/4bfb6893e7840b748648b749b22078f2facfee0a/common-lisp/lisp-reader/package-def.lisp
;;;; for a list of all CLs symbols

(cl:in-package #:common-lisp-user)

(defpackage #:breeze.cl
  (:documentation "Some useful metadata on cl's symbols")
  (:use :cl)
  (:export #:higher-order-function-p))

(in-package #:breeze.cl)

(defun higher-order-function-p (function)
  (multiple-value-bind (position _)
      (gethash function
               #.(alexandria:alist-hash-table '((funcall . 0) (map-into . 1) (mapcon . 0) (mapl . 0) (set-pprint-dispatch . 1)
                                                (mapcar . 0) (reduce . 0) (maplist . 0) (complement . 0)
                                                (shared-initialize . 17) (map . 1) (mapcan . 0) (set-macro-character . 1)
                                                (mapc . 0) (set-dispatch-macro-character . 2) (apply . 0))))
    (declare (ignore _))
    position))



#++
(higher-order-function-p 'mapcar)
;; => 0

#++
(higher-order-function-p '+)
;; => nil


#++
(do-external-symbols (s :cl) nil (when (ignore-errors (symbol-function s))
                                   (print s)))

;;;; N.B. Some symbols are there multiple times because multiple
;;;; groupings are possible

;; TODO see ./cl-todo.lisp for the missing ones
;; TODO better name?
(defparameter *seed*
  `(("Math"
     ("Arithmetic" (* + - / 1+ 1- MOD REM))
     ("Comparison" (/= < <= = > >=))
     ("Complex numbers"
      (CONJUGATE IMAGPART REALP REALPART
                 COMPLEX COMPLEXP PHASE UPGRADED-COMPLEX-PART-TYPE))
     ("Trigonometry"
      (ACOS ACOSH COS COSH ASIN ASINH ATAN ATANH CIS SIN SINH TAN
            TANH))
     ("Rational numbers"
      (DENOMINATOR NUMERATOR RATIONAL RATIONALIZE RATIONALP))
     (GCD LCM)
     ("Rounding"
      (CEILING FFLOOR FLOOR FCEILING ROUND FROUND FTRUNCATE TRUNCATE))
     (SQRT ISQRT EXPT EXP)
     ;; all predicates on numbers
     (PLUSP MINUSP ZEROP EVENP ODDP NUMBERP INTEGERP FLOATP)

     (MAX MIN)
     (MINUSP PLUSP ABS)
     (INTEGER-DECODE-FLOAT
      INTEGER-LENGTH
      INTEGERP)
     ("Float"
      (FLOAT
       DECODE-FLOAT
       FLOAT-DIGITS
       FLOAT-PRECISION
       FLOAT-RADIX
       FLOAT-SIGN
       FLOATP
       SCALE-FLOAT))
     PARSE-INTEGER)

    (("Bit-wise logical operations on bit-arrays."
      BIT-AND
      BIT-ANDC1
      BIT-ANDC2
      BIT-EQV
      BIT-IOR
      BIT-NAND
      BIT-NOR
      BIT-NOT
      BIT-ORC1
      BIT-ORC2
      BIT-VECTOR-P
      BIT-XOR)
     BYTE
     BYTE-POSITION
     BYTE-SIZE
     BOOLE
     ASH)

    ("environment"
     (USER-HOMEDIR-PATHNAME
      HOST-NAMESTRING
      ROOM)
     (DECLAIM PROCLAIM)
     ((BOUNDP MAKUNBOUND)
      (FBOUNDP FMAKUNBOUND)
      (FDEFINITION FUNCTION-LAMBDA-EXPRESSION)
      FUNCTION-KEYWORDS
      (FUNCTION FUNCTIONP))
     (APROPOS APROPOS-LIST))

    ("list as set"
     SUBSETP
     (UNION NUNION)
     (INTERSECTION NINTERSECTION)
     (SET-DIFFERENCE NSET-DIFFERENCE)
     (SET-EXCLUSIVE-OR NSET-EXCLUSIVE-OR))


    ;; list constructors
    CONS
    ACONS
    ADJOIN
    APPEND
    BUTLAST
    REVAPPEND
    LDIFF

    ;; list "setf"
    PUSH
    PUSHNEW
    POP

    ;; list "mutator"
    NBUTLAST
    NCONC

    ;; list predicates
    TAILP
    ATOM
    CONSP
    NULL

    ("list readers"
     LENGTH
     GETF
     LAST
     NTH
     NTHCDR
     REST
     (FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINTH TENTH)
     (CAAAAR CAAADR CAAAR CAADAR CAADDR CAADR CAAR CADAAR CADADR
             CADAR CADDAR CADDDR CADDR CADR CAR CDAAAR CDAADR CDAAR CDADAR
             CDADDR CDADR CDAR CDDAAR CDDADR CDDAR CDDDAR CDDDDR CDDDR
             CDDR CDR))

    ("alist" ASSOC ASSOC-IF ASSOC-IF-NOT
             RASSOC RASSOC-IF RASSOC-IF-NOT
             NSUBLIS SUBLIS
             PAIRLIS)
    ("tree" ((NSUBST NSUBST-IF NSUBST-IF-NOT)
             (SUBST SUBST-IF SUBST-IF-NOT))
            TREE-EQUAL
            COPY-TREE)

    ("Sequence"
     ("Array"
      UPGRADED-ARRAY-ELEMENT-TYPE
      (AREF (ARRAY-ROW-MAJOR-INDEX ROW-MAJOR-AREF))
      (ADJUST-ARRAY ADJUSTABLE-ARRAY-P ARRAY-HAS-FILL-POINTER-P FILL-POINTER)
      ((ARRAY-IN-BOUNDS-P
        ARRAY-RANK
        ARRAY-TOTAL-SIZE)
       (ARRAY-DIMENSION ARRAY-DIMENSIONS))
      ARRAY-DISPLACEMENT
      ARRAY-ELEMENT-TYPE
      ARRAYP
      ("bit array" SBIT BIT)
      ("vector"
       VECTOR
       VECTOR-POP
       VECTOR-PUSH
       VECTOR-PUSH-EXTEND
       VECTORP
       ("simple vector" SVREF)))
     MAKE-SEQUENCE
     (NSUBSTITUTE NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT)
     (SUBSTITUTE SUBSTITUTE-IF SUBSTITUTE-IF-NOT)
     SUBSEQ
     CONCATENATE
     SEARCH
     MISMATCH
     ELT
     REPLACE
     (REMOVE REMOVE-DUPLICATES (REMOVE-IF REMOVE-IF-NOT))
     (DELETE DELETE-DUPLICATES (DELETE-IF DELETE-IF-NOT))
     (COUNT COUNT-IF COUNT-IF-NOT)
     (POSITION POSITION-IF POSITION-IF-NOT)
     REVERSE)

    RPLACA
    RPLACD

    ("CLOS"
     METHOD-COMBINATION-ERROR
     METHOD-QUALIFIERS
     (WITH-ACCESSORS WITH-SLOTS)
     UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
     UPDATE-INSTANCE-FOR-REDEFINED-CLASS
     UNBOUND-SLOT-INSTANCE
     REINITIALIZE-INSTANCE
     REMOVE-METHOD
     NO-APPLICABLE-METHOD
     NO-NEXT-METHOD
     INVALID-METHOD-ERROR
     TYPE-ERROR-DATUM
     TYPE-ERROR-EXPECTED-TYPE
     INITIALIZE-INSTANCE
     CLASS-NAME
     CLASS-OF
     ALLOCATE-INSTANCE
     CALL-METHOD
     ADD-METHOD
     ENSURE-GENERIC-FUNCTION
     CHANGE-CLASS
     COMPUTE-APPLICABLE-METHODS)

    ;; Control flow
    (AND OR)
    (COND UNLESS IF WHEN)
    (CASE CCASE ECASE)
    (CTYPECASE TYPECASE)

    (TAGBODY GO)
    (BLOCK RETURN RETURN-FROM)
    UNWIND-PROTECT

    (CATCH THROW)

    ;; Errors
    (UNBOUND-SLOT-INSTANCE
     NO-APPLICABLE-METHOD
     NO-NEXT-METHOD
     INVALID-METHOD-ERROR
     IGNORE-ERRORS
     CELL-ERROR-NAME
     CERROR
     ASSERT
     ARITHMETIC-ERROR-OPERANDS
     ARITHMETIC-ERROR-OPERATION
     CHECK-TYPE
     FILE-ERROR-PATHNAME)

    ("Characters"
     NAME-CHAR
     (DIGIT-CHAR DIGIT-CHAR-P)
     ((UPPER-CASE-P BOTH-CASE-P)
      (CHAR-UPCASE CHAR-DOWNCASE))
     (CODE-CHAR CHAR-CODE CHAR-INT)
     ("Predicates"
      (ALPHA-CHAR-P ALPHANUMERICP)
      GRAPHIC-CHAR-P)

     CHAR-NAME
     ("Comparison"
      ("Not case sensitive"
       (CHAR-EQUAL CHAR-GREATERP CHAR-LESSP CHAR-NOT-EQUAL CHAR-NOT-GREATERP CHAR-NOT-LESSP))
      ("Case sensitive"
       CHAR/= CHAR< CHAR<= CHAR= CHAR> CHAR>=))


     CHARACTER
     CHARACTERP
     STANDARD-CHAR-P)

    ("Higher order functions (HoFs)"
     APPLY
     COMPLEMENT
     FUNCALL
     CONSTANTLY
     REDUCE)

    ("Hash-tables"
     WITH-HASH-TABLE-ITERATOR
     REMHASH
     SXHASH
     CLRHASH
     GETHASH
     HASH-TABLE-COUNT
     HASH-TABLE-P
     HASH-TABLE-REHASH-SIZE
     HASH-TABLE-REHASH-THRESHOLD
     HASH-TABLE-SIZE
     HASH-TABLE-TEST)

    ("Compilation"
     COMPILE
     COMPILE-FILE
     COMPILE-FILE-PATHNAME
     COMPILED-FUNCTION-P
     COMPILER-MACRO-FUNCTION
     DEFINE-COMPILER-MACRO
     WITH-COMPILATION-UNIT)

    ;; stream
    ("Streams"
     (INPUT-STREAM-P OUTPUT-STREAM-P)
     (OPEN CLOSE OPEN-STREAM-P)
     (CLEAR-INPUT CLEAR-OUTPUT)
     SYNONYM-STREAM-SYMBOL
     MAKE-BROADCAST-STREAM
     MAKE-CONCATENATED-STREAM
     MAKE-ECHO-STREAM
     INTERACTIVE-STREAM-P
     STREAM-ELEMENT-TYPE
     STREAM-ERROR-STREAM
     STREAM-EXTERNAL-FORMAT
     STREAMP
     WITH-INPUT-FROM-STRING
     MAKE-SYNONYM-STREAM
     MAKE-TWO-WAY-STREAM
     MAKE-STRING-INPUT-STREAM
     MAKE-STRING-OUTPUT-STREAM
     BROADCAST-STREAM-STREAMS
     CONCATENATED-STREAM-STREAMS
     ECHO-STREAM-INPUT-STREAM
     ECHO-STREAM-OUTPUT-STREAM
     GET-OUTPUT-STREAM-STRING
     TWO-WAY-STREAM-INPUT-STREAM
     TWO-WAY-STREAM-OUTPUT-STREAM
     WITH-OPEN-STREAM
     (PEEK-CHAR LISTEN UNREAD-CHAR)
     WITH-OUTPUT-TO-STRING
     WITH-STANDARD-IO-SYNTAX
     (WRITE
      WRITE-BYTE
      WRITE-CHAR
      WRITE-LINE
      WRITE-SEQUENCE
      WRITE-STRING
      WRITE-TO-STRING)
     (FORMAT FORMATTER)
     ("Interactions"
      Y-OR-N-P YES-OR-NO-P LISTEN)
     ("Flushing and newlines"
      FORCE-OUTPUT (FRESH-LINE TERPRI)))

    ("plist"
     ("symbol plist" GET)
     REMF)

    ("Symbol"
     SYMBOL-FUNCTION
     SYMBOL-MACROLET
     SYMBOL-NAME
     SYMBOL-PACKAGE
     SYMBOL-PLIST
     SYMBOL-VALUE
     SYMBOLP
     KEYWORDP
     MAKE-SYMBOL
     MAKUNBOUND
     FMAKUNBOUND)

    ("type"
     COERCE
     SUBTYPEP
     TYPE-OF
     TYPEP
     CHECK-TYPE
     ;; Special forms
     THE
     DEFTYPE)

    ("Generalized references"
     (DECF INCF)
     DEFINE-MODIFY-MACRO
     GET-SETF-EXPANSION
     DEFINE-SETF-EXPANDER
     DEFSETF
     (SETF PSETF)
     PSETQ)

    ("Files"
     WITH-OPEN-FILE
     (DELETE-FILE RENAME-FILE PROBE-FILE)
     FILE-AUTHOR
     FILE-ERROR-PATHNAME
     FILE-LENGTH
     FILE-NAMESTRING
     FILE-POSITION
     FILE-STRING-LENGTH
     FILE-WRITE-DATE)

    ("Time"
     (DECODE-UNIVERSAL-TIME ENCODE-UNIVERSAL-TIME GET-DECODED-TIME)
     GET-UNIVERSAL-TIME
     (GET-INTERNAL-REAL-TIME GET-INTERNAL-RUN-TIME))

    ("Reader"
     WITH-STANDARD-IO-SYNTAX

     ("Read table"
      READTABLE-CASE READTABLEP COPY-READTABLE)

     ("reader macros"
      (GET-DISPATCH-MACRO-CHARACTER
       SET-DISPATCH-MACRO-CHARACTER
       MAKE-DISPATCH-MACRO-CHARACTER)
      (GET-MACRO-CHARACTER SET-MACRO-CHARACTER)
      SET-SYNTAX-FROM-CHAR))

    ("symbol" INTERN UNINTERN GENSYM GENTEMP)

    ("debugger"
     BREAK INVOKE-DEBUGGER STEP)

    (ABORT CONTINUE USE-VALUE MUFFLE-WARNING STORE-VALUE)
    (HANDLER-BIND HANDLER-CASE)
    ((INVOKE-RESTART INVOKE-RESTART-INTERACTIVELY)
     (FIND-RESTART COMPUTE-RESTARTS)
     (RESTART-BIND RESTART-CASE))
    RESTART-NAME
    (WITH-CONDITION-RESTARTS WITH-SIMPLE-RESTART)

    (DEFINE-CONDITION (WARN MUFFLE-WARNING) SIGNAL)

    (LABELS FLET LAMBDA DEFUN)

    ("Random number generator"
     RANDOM RANDOM-STATE-P)

    ("Strings"
     NSTRING-CAPITALIZE
     NSTRING-DOWNCASE
     NSTRING-UPCASE
     STRING
     STRING-CAPITALIZE
     STRING-DOWNCASE
     (STRING-EQUAL
      STRING-GREATERP
      STRING-LEFT-TRIM
      STRING-LESSP
      STRING-NOT-EQUAL
      STRING-NOT-GREATERP
      STRING-NOT-LESSP
      STRING-RIGHT-TRIM
      STRING-TRIM)
     STRING-UPCASE
     (STRING/=
      STRING<
      STRING<=
      STRING=
      STRING>
      STRING>=)
     ("Predicates"
      STRINGP SIMPLE-STRING-P)
     ("Accessor"
      CHAR SCHAR))

    ("multiple values"
     VALUES
     VALUES-LIST
     NTH-VALUE
     MULTIPLE-VALUE-BIND
     MULTIPLE-VALUE-CALL
     MULTIPLE-VALUE-LIST
     MULTIPLE-VALUE-PROG1
     MULTIPLE-VALUE-SETQ)

    ("Package"
     (WITH-PACKAGE-ITERATOR LIST-ALL-PACKAGES)
     (DELETE-PACKAGE RENAME-PACKAGE)
     EXPORT
     UNEXPORT
     DEFPACKAGE
     (UNUSE-PACKAGE USE-PACKAGE PACKAGE-USE-LIST
                    PACKAGE-USED-BY-LIST)
     MAKE-PACKAGE
     PACKAGE-ERROR-PACKAGE
     PACKAGE-NAME
     PACKAGE-NICKNAMES
     PACKAGE-SHADOWING-SYMBOLS
     PACKAGEP)

    ("paths"
     TRANSLATE-PATHNAME
     TRUENAME
     (TRANSLATE-LOGICAL-PATHNAME
      LOGICAL-PATHNAME
      LOGICAL-PATHNAME-TRANSLATIONS)
     USER-HOMEDIR-PATHNAME
     WILD-PATHNAME-P
     MERGE-PATHNAMES
     PARSE-NAMESTRING
     PATHNAME
     PATHNAME-DEVICE
     PATHNAME-DIRECTORY
     PATHNAME-HOST
     PATHNAME-MATCH-P
     PATHNAME-NAME
     PATHNAME-TYPE
     PATHNAME-VERSION
     PATHNAMEP)

    ("Printing"
     ("Pretty printer"
      PPRINT
      PPRINT-DISPATCH
      PPRINT-EXIT-IF-LIST-EXHAUSTED
      PPRINT-FILL
      PPRINT-INDENT
      PPRINT-LINEAR
      PPRINT-LOGICAL-BLOCK
      PPRINT-NEWLINE
      PPRINT-POP
      PPRINT-TAB
      PPRINT-TABULAR)
     PRIN1
     PRIN1-TO-STRING
     PRINC
     PRINC-TO-STRING
     PRINT
     PRINT-NOT-READABLE-OBJECT
     PRINT-OBJECT
     PRINT-UNREADABLE-OBJECT)

;;; equivalences
    (== (simple-string-p object)
        (typep object 'simple-string))))

#++
(labels ((walk (list crumbs)
           (loop
             :for el :in list
             :do
                (cond
                  ((stringp el)
                   (push el crumbs))
                  ((atom el)
                   (format t "~&~s ~{~s~^, ~}" el crumbs)
                   (when (symbol-function el)
                     (print (sb-impl::%fun-ftype (symbol-function el)))))
                  ((and (listp el)
                        (eq '== (car el)))
                   (format t "~&equivalence: ~s == ~s" (second el) (third el)))
                  ((and (listp el)
                        (stringp (car el))
                        (walk (cdr el) `(,(car el) ,@crumbs))))
                  (t (walk el crumbs))))))
  (walk *seed* nil))
