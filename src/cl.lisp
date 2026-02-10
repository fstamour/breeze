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
               #.(alexandria:alist-hash-table
                  '((funcall . 0) (map-into . 1) (mapcon . 0) (mapl . 0) (set-pprint-dispatch . 1)
                    (mapcar . 0) (reduce . 0) (maplist . 0) (complement . 0)
                    (shared-initialize . 17) (map . 1) (mapcan . 0) (set-macro-character . 1)
                    (mapc . 0) (set-dispatch-macro-character . 2) (apply . 0))))
    (declare (ignore _))
    position))

(defun unsafep (form)
  (and (not (atom form))))

;; (unsafep 42)

#++
(higher-order-function-p 'mapcar)
;; => 0

#++
(higher-order-function-p '+)
;; => nil

(defvar *declarations* (make-hash-table :test 'equal))

;; name + kind is analogous to cl:documentation's x and doc-type
(defmacro decl ((name kind)
                &body details)
  `(let ((key '(,name ,kind)))
     ;; TODO validate/parse the details; use actual objects
     (setf (gethash key *declarations*) ',details)))

;; the "->" should be optional, e.g. just for making it easier to read

;; sbcl has a nice "(modifying type-specifier)" annotation

(decl (+ function)
  (function (&rest number) -> number)
  ;; TODO technically, it's not commutative or associative for
  ;; floating point numbers
  :associative t
  :commutative t
  :identity-element 0
  :inverse -
  :safe t)

(decl (make-hash-table function)
  (function (&key
             test
             size
             rehash-size
             rehash-threshold
             :+sbcl hash-function
             :+sbcl weakness
             :+sbcl synchronized) -> (hash-table)
            (where ((test (member eq eql equal equalp))
                    (size (integer 0))
                    (rehash-size (or (integer 1 *) (float (1.0) *)))
                    (rehash-threshold (real 0 1)))))
  :allocate t)

(decl (coerce function)
  (function (t type-specifier) -> t)
  :safe t
  :allocate :probably)

#++
(defalias function car first)

(decl (car function)
  :arguments ((cons ?a ?b))
  :return ?a
  :alias first
  :setf-able t
  :safe t
  :allocate nil
  :identity-element nil)

#++
(do-external-symbols (s :cl) nil (when (ignore-errors (symbol-function s))
                                   (print s)))

;;;; N.B. Some symbols are there multiple times because multiple
;;;; groupings are possible

(defparameter *groups*
  `(("Math"
     ("Arithmetic"
      (+ - * / 1+ 1- MOD REM))
     ("Comparison" (/= < <= = > >=))
     ("Complex numbers"
      (CONJUGATE IMAGPART REALP REALPART
                 COMPLEX COMPLEXP PHASE UPGRADED-COMPLEX-PART-TYPE))
     ("Trigonometry"
      (ACOS ACOSH COS COSH ASIN ASINH ATAN ATANH CIS SIN SINH TAN
            TANH))
     ("Rational numbers"
      (DENOMINATOR NUMERATOR RATIONAL RATIONALIZE RATIONALP))
     ("Greatest common divisor and least common multiple"
      (GCD LCM))
     ("Rounding"
      (CEILING FFLOOR FLOOR FCEILING ROUND FROUND FTRUNCATE TRUNCATE))
     ("Exponentiation and logarithms"
      (SQRT ISQRT EXPT EXP LOG))
     ("Number predicates"
      (PLUSP MINUSP ZEROP EVENP ODDP NUMBERP INTEGERP FLOATP))

     ("Extrema" (MAX MIN))
     ("Sign"
      (MINUSP PLUSP ABS SIGNUM))
     ("Integer representation"
      (INTEGER-DECODE-FLOAT
       INTEGER-LENGTH
       PARSE-INTEGER))
     ("Float"
      (FLOAT
       DECODE-FLOAT
       FLOAT-DIGITS
       FLOAT-PRECISION
       FLOAT-RADIX
       FLOAT-SIGN
       FLOATP
       SCALE-FLOAT))
     )

    ("Bit operations"
     ("Bit arrays"
      ;; These are both functions (see https://www.lispworks.com/documentation/HyperSpec/Body/f_bt_and.htm)
      ;; and "constants" used by the function `boole' (see https://www.lispworks.com/documentation/HyperSpec/Body/f_boole.htm)
      BIT-AND BIT-ANDC1 BIT-ANDC2 BIT-EQV BIT-IOR BIT-NAND
      BIT-NOR BIT-NOT BIT-ORC1 BIT-ORC2 BIT-VECTOR-P BIT-XOR)
     ("Byte operations"
      BYTE BYTE-POSITION BYTE-SIZE
      LDB LDB-TEST DEPOSIT-FIELD DPB MASK-FIELD)
     ("Logical operations on integers"
      ;; BOOLE is a unified dispatch interface for these same operations
      ;; See https://www.lispworks.com/documentation/HyperSpec/Body/f_boole.htm
      BOOLE
      LOGAND LOGANDC1 LOGANDC2 LOGBITP LOGCOUNT LOGEQV LOGIOR
      LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2 LOGTEST LOGXOR
      ASH))

    ("Environment"
     ("System information"
      (USER-HOMEDIR-PATHNAME HOST-NAMESTRING ROOM)
      (LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION)
      (MACHINE-INSTANCE MACHINE-TYPE MACHINE-VERSION)
      (SOFTWARE-TYPE SOFTWARE-VERSION)
      (SHORT-SITE-NAME LONG-SITE-NAME))
     ("Declarations"
      (DECLAIM PROCLAIM LOCALLY))
     ("Bindings and definitions"
      (BOUNDP MAKUNBOUND)
      (FBOUNDP FMAKUNBOUND)
      (FDEFINITION FUNCTION-LAMBDA-EXPRESSION)
      FUNCTION-KEYWORDS
      (FUNCTION FUNCTIONP))
     ("Introspection"
      ("Search" (APROPOS APROPOS-LIST))
      (INSPECT DESCRIBE DESCRIBE-OBJECT DOCUMENTATION DISASSEMBLE)
      (CONSTANTP SPECIAL-OPERATOR-P))
     ("Session recording" DRIBBLE)
     ("Editor" ED)
     SLEEP)

    ("Comparison"
     ("Equality"
      (EQ EQL EQUAL EQUALP)
      (CHAR= CHAR-EQUAL CHAR/= CHAR-NOT-EQUAL)
      (STRING= STRING-EQUAL STRING/= STRING-NOT-EQUAL)
      (= /=)
      TREE-EQUAL)
     ("Ordering"
      (< <= > >=)
      (CHAR< CHAR<= CHAR> CHAR>=)
      (CHAR-LESSP CHAR-NOT-LESSP CHAR-GREATERP CHAR-NOT-GREATERP)
      (STRING< STRING<= STRING> STRING>=)
      (STRING-LESSP STRING-NOT-LESSP STRING-GREATERP STRING-NOT-GREATERP)))

    ("List as set"
     SUBSETP
     (UNION NUNION)
     (INTERSECTION NINTERSECTION)
     (SET-DIFFERENCE NSET-DIFFERENCE)
     (SET-EXCLUSIVE-OR NSET-EXCLUSIVE-OR))


    ("List constructors"
     CONS
     ACONS
     ADJOIN
     APPEND
     (LIST LIST*)
     MAKE-LIST
     COPY-LIST
     BUTLAST
     REVAPPEND
     LDIFF)

    ("List place operators"
     PUSH
     PUSHNEW
     POP)

    ("List mutators"
     NBUTLAST
     NCONC
     NRECONC
     RPLACA
     RPLACD)

    ("List predicates"
     TAILP
     ATOM
     CONSP
     LISTP
     ENDP
     NULL)

    ("List navigation"
     LAST
     NTH
     NTHCDR
     REST
     (FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINTH TENTH)
     (CAAAAR CAAADR CAAAR CAADAR CAADDR CAADR CAAR CADAAR CADADR
             CADAR CADDAR CADDDR CADDR CADR CAR CDAAAR CDAADR CDAAR CDADAR
             CDADDR CDADR CDAR CDDAAR CDDADR CDDAR CDDDAR CDDDDR CDDDR
             CDDR CDR))

    ("List places"
     GETF
     NTH
     REST
     (FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINTH TENTH)
     (CAAAAR CAAADR CAAAR CAADAR CAADDR CAADR CAAR CADAAR CADADR
             CADAR CADDAR CADDDR CADDR CADR CAR CDAAAR CDAADR CDAAR CDADAR
             CDADDR CDADR CDAR CDDAAR CDDADR CDDAR CDDDAR CDDDDR CDDDR
             CDDR CDR))

    ("List length"
     LENGTH
     LIST-LENGTH)

    ("Alist" ASSOC ASSOC-IF ASSOC-IF-NOT
             RASSOC RASSOC-IF RASSOC-IF-NOT
             NSUBLIS SUBLIS
             PAIRLIS)
    ("Tree" ((NSUBST NSUBST-IF NSUBST-IF-NOT)
             (SUBST SUBST-IF SUBST-IF-NOT))
            TREE-EQUAL
            COPY-TREE)

    ("Copy"
     COPY-LIST COPY-ALIST COPY-TREE COPY-SEQ
     COPY-STRUCTURE COPY-SYMBOL
     COPY-READTABLE COPY-PPRINT-DISPATCH)

    ("Sequence"
     ("Array"
      MAKE-ARRAY
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
      ("Bit array" SBIT BIT SIMPLE-BIT-VECTOR-P)
      ("Vector"
       VECTOR
       VECTOR-POP
       VECTOR-PUSH
       VECTOR-PUSH-EXTEND
       (VECTORP SIMPLE-VECTOR-P SIMPLE-BIT-VECTOR-P)
       ("Simple vector" SVREF)))
     MAKE-SEQUENCE
     (NSUBSTITUTE NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT)
     (SUBSTITUTE SUBSTITUTE-IF SUBSTITUTE-IF-NOT)
     SUBSEQ
     CONCATENATE
     ("Search"
      SEARCH
      (FIND FIND-IF FIND-IF-NOT)
      (POSITION POSITION-IF POSITION-IF-NOT)
      (MEMBER MEMBER-IF MEMBER-IF-NOT))
     ("Diffing"
      MISMATCH TAILP LDIFF SUBSETP TREE-EQUAL)
     ELT
     REPLACE
     (REMOVE REMOVE-DUPLICATES (REMOVE-IF REMOVE-IF-NOT))
     (DELETE DELETE-DUPLICATES (DELETE-IF DELETE-IF-NOT))
     (COUNT COUNT-IF COUNT-IF-NOT)
     FILL
     (SORT STABLE-SORT MERGE)
     (REVERSE NREVERSE REVAPPEND NRECONC)
     COPY-SEQ
     ("Sequence predicates"
      EVERY SOME NOTANY NOTEVERY))

    ("Type definition"
     DEFCLASS DEFSTRUCT DEFTYPE DEFINE-CONDITION)

    ("Structures"
     DEFSTRUCT COPY-STRUCTURE)

    ("CLOS"
     ("Classes"
      DEFCLASS CLASS-NAME CLASS-OF FIND-CLASS)
     ("Generic functions"
      DEFGENERIC DEFMETHOD ENSURE-GENERIC-FUNCTION)
     ("Methods"
      ADD-METHOD REMOVE-METHOD CALL-METHOD
      FIND-METHOD METHOD-QUALIFIERS
      DEFINE-METHOD-COMBINATION METHOD-COMBINATION-ERROR
      COMPUTE-APPLICABLE-METHODS)
     ("Instance creation"
      MAKE-INSTANCE ALLOCATE-INSTANCE
      INITIALIZE-INSTANCE SHARED-INITIALIZE REINITIALIZE-INSTANCE)
     ("Instance update"
      UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
      UPDATE-INSTANCE-FOR-REDEFINED-CLASS
      MAKE-INSTANCES-OBSOLETE
      CHANGE-CLASS)
     ("Slots"
      SLOT-BOUNDP SLOT-EXISTS-P SLOT-MAKUNBOUND
      SLOT-MISSING SLOT-UNBOUND SLOT-VALUE
      UNBOUND-SLOT-INSTANCE)
     ("Convenience macros"
      WITH-ACCESSORS WITH-SLOTS)
     ("Conditions"
      NO-APPLICABLE-METHOD NO-NEXT-METHOD INVALID-METHOD-ERROR)
     MAKE-LOAD-FORM-SAVING-SLOTS)

    ("Binding"
     (LET LET*)
     MULTIPLE-VALUE-BIND
     DESTRUCTURING-BIND
     (PROG PROG* PROGV))

    ("Boolean logic"
     (NOT AND OR))

    ("Control flow"
     (AND OR)
     (COND UNLESS IF WHEN)
     (CASE CCASE ECASE)
     (TYPECASE CTYPECASE ETYPECASE)

     (TAGBODY GO)
     (BLOCK RETURN RETURN-FROM)
     UNWIND-PROTECT

     (CATCH THROW)

     ("Sequencing" PROG1 PROG2 PROGN))

    ("Condition types"
     ("Base hierarchy"
      CONDITION SERIOUS-CONDITION ERROR WARNING STYLE-WARNING STORAGE-CONDITION)
     ("Simple variants"
      (SIMPLE-CONDITION SIMPLE-CONDITION-FORMAT-CONTROL SIMPLE-CONDITION-FORMAT-ARGUMENTS)
      SIMPLE-ERROR SIMPLE-WARNING SIMPLE-TYPE-ERROR)
     ("File conditions"
      (FILE-ERROR FILE-ERROR-PATHNAME))
     ("Package conditions"
      (PACKAGE-ERROR PACKAGE-ERROR-PACKAGE))
     ("Print conditions"
      (PRINT-NOT-READABLE PRINT-NOT-READABLE-OBJECT))
     ("Method conditions"
      NO-APPLICABLE-METHOD NO-NEXT-METHOD INVALID-METHOD-ERROR)
     ("Slot conditions"
      (UNBOUND-SLOT UNBOUND-SLOT-INSTANCE))
     ("Control and program errors"
      CONTROL-ERROR PROGRAM-ERROR)
     ("Type errors"
      (TYPE-ERROR TYPE-ERROR-DATUM TYPE-ERROR-EXPECTED-TYPE)
      SIMPLE-TYPE-ERROR)
     ("Variable and function errors"
      (CELL-ERROR CELL-ERROR-NAME)
      UNBOUND-VARIABLE UNDEFINED-FUNCTION)
     ("Arithmetic conditions"
      (ARITHMETIC-ERROR ARITHMETIC-ERROR-OPERANDS ARITHMETIC-ERROR-OPERATION)
      DIVISION-BY-ZERO
      FLOATING-POINT-INEXACT FLOATING-POINT-INVALID-OPERATION
      FLOATING-POINT-OVERFLOW FLOATING-POINT-UNDERFLOW)
     ("Stream and reader conditions"
      (STREAM-ERROR STREAM-ERROR-STREAM)
      END-OF-FILE READER-ERROR PARSE-ERROR))

    ("Signaling and assertion"
     ASSERT CHECK-TYPE IGNORE-ERRORS)

    ("Characters"
     CHARACTER
     (CHAR-NAME NAME-CHAR)
     (DIGIT-CHAR DIGIT-CHAR-P)
     ("Case conversion"
      (UPPER-CASE-P LOWER-CASE-P BOTH-CASE-P)
      (CHAR-UPCASE CHAR-DOWNCASE))
     ("Character codes"
      CODE-CHAR CHAR-CODE CHAR-INT)
     ("Predicates"
      CHARACTERP STANDARD-CHAR-P
      (ALPHA-CHAR-P ALPHANUMERICP)
      GRAPHIC-CHAR-P)
     ("Comparison"
      ("Not case sensitive"
       (CHAR-EQUAL CHAR-GREATERP CHAR-LESSP CHAR-NOT-EQUAL CHAR-NOT-GREATERP CHAR-NOT-LESSP))
      ("Case sensitive"
       CHAR/= CHAR< CHAR<= CHAR= CHAR> CHAR>=)))

    ("Higher order functions (HoFs)"
     APPLY
     COMPLEMENT
     FUNCALL
     CONSTANTLY
     REDUCE)

    ("Iteration"
     (LOOP LOOP-FINISH)
     (DO DO* DOLIST DOTIMES)
     REDUCE
     ("Mapping"
      MAP MAP-INTO MAPC MAPCAN MAPCAR MAPCON MAPL MAPLIST
      MAPHASH WITH-HASH-TABLE-ITERATOR)
     ("Mapping packages"
      (DO-ALL-SYMBOLS DO-EXTERNAL-SYMBOLS DO-SYMBOLS)
      WITH-PACKAGE-ITERATOR))

    ("Hash-tables"
     MAKE-HASH-TABLE
     MAPHASH
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

    ("Compilation and evaluation"
     COMPILE
     COMPILE-FILE
     COMPILE-FILE-PATHNAME
     COMPILED-FUNCTION-P
     COMPILER-MACRO-FUNCTION
     DEFINE-COMPILER-MACRO
     WITH-COMPILATION-UNIT
     (EVAL LOAD)
     (MAKE-LOAD-FORM MAKE-LOAD-FORM-SAVING-SLOTS)
     DISASSEMBLE)

    ("Evaluation control"
     QUOTE EVAL-WHEN LOAD-TIME-VALUE

    ("Streams"
     ("Predicates"
      STREAMP INPUT-STREAM-P OUTPUT-STREAM-P
      OPEN-STREAM-P INTERACTIVE-STREAM-P)
     ("Lifecycle"
      OPEN CLOSE)
     ("Stream constructors"
      MAKE-BROADCAST-STREAM MAKE-CONCATENATED-STREAM
      MAKE-ECHO-STREAM MAKE-SYNONYM-STREAM MAKE-TWO-WAY-STREAM
      MAKE-STRING-INPUT-STREAM MAKE-STRING-OUTPUT-STREAM)
     ("Stream accessors"
      STREAM-ELEMENT-TYPE STREAM-ERROR-STREAM STREAM-EXTERNAL-FORMAT
      SYNONYM-STREAM-SYMBOL
      BROADCAST-STREAM-STREAMS CONCATENATED-STREAM-STREAMS
      ECHO-STREAM-INPUT-STREAM ECHO-STREAM-OUTPUT-STREAM
      TWO-WAY-STREAM-INPUT-STREAM TWO-WAY-STREAM-OUTPUT-STREAM
      GET-OUTPUT-STREAM-STRING)
     ("Convenience macros"
      WITH-INPUT-FROM-STRING WITH-OUTPUT-TO-STRING
      WITH-OPEN-STREAM WITH-STANDARD-IO-SYNTAX)
     ("Writing"
      WRITE WRITE-BYTE WRITE-CHAR WRITE-LINE
      WRITE-SEQUENCE WRITE-STRING WRITE-TO-STRING
      (FORMAT FORMATTER))
     ("Reading"
      READ READ-BYTE READ-CHAR READ-CHAR-NO-HANG
      READ-DELIMITED-LIST READ-FROM-STRING READ-LINE
      READ-PRESERVING-WHITESPACE READ-SEQUENCE)
     ("Buffering"
      CLEAR-INPUT CLEAR-OUTPUT
      PEEK-CHAR LISTEN UNREAD-CHAR
      FORCE-OUTPUT FINISH-OUTPUT)
     ("Newlines"
      FRESH-LINE TERPRI)
     ("Interactions"
      Y-OR-N-P YES-OR-NO-P LISTEN))

    ("Plist"
     ("Symbol plist" GET REMPROP)
     GET-PROPERTIES
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
     (MAKE-SYMBOL COPY-SYMBOL)
     MAKUNBOUND
     FMAKUNBOUND
     (GENSYM GENTEMP)
     (INTERN UNINTERN))

    ("Type"
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
     (ROTATEF SHIFTF)
     ((SETQ PSETQ) SET))

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

    ("Directories"
     DIRECTORY
     DIRECTORY-NAMESTRING
     ENSURE-DIRECTORIES-EXIST)

    ("Time"
     (DECODE-UNIVERSAL-TIME ENCODE-UNIVERSAL-TIME GET-DECODED-TIME)
     GET-UNIVERSAL-TIME
     (GET-INTERNAL-REAL-TIME GET-INTERNAL-RUN-TIME))

    ("Reader"
     WITH-STANDARD-IO-SYNTAX

     ("Read table"
      READTABLE-CASE READTABLEP COPY-READTABLE)

     ("Reader macros"
      (GET-DISPATCH-MACRO-CHARACTER
       SET-DISPATCH-MACRO-CHARACTER
       MAKE-DISPATCH-MACRO-CHARACTER)
      (GET-MACRO-CHARACTER SET-MACRO-CHARACTER)
      SET-SYNTAX-FROM-CHAR))

    ("Debugger"
     BREAK INVOKE-DEBUGGER STEP
     (TRACE UNTRACE)
     TIME)

    ("Condition system"
     (ERROR CERROR SIGNAL (WARN MUFFLE-WARNING))
     MAKE-CONDITION
     DEFINE-CONDITION
     (SIMPLE-CONDITION-FORMAT-ARGUMENTS SIMPLE-CONDITION-FORMAT-CONTROL)
     (HANDLER-BIND HANDLER-CASE)
     (ABORT CONTINUE USE-VALUE MUFFLE-WARNING STORE-VALUE)
     ((INVOKE-RESTART INVOKE-RESTART-INTERACTIVELY)
      (FIND-RESTART COMPUTE-RESTARTS)
      (RESTART-BIND RESTART-CASE))
     RESTART-NAME
     (WITH-CONDITION-RESTARTS WITH-SIMPLE-RESTART))

    ("Function definition"
     (LABELS FLET LAMBDA DEFUN))

    ("Macro"
     (DEFMACRO MACROLET DEFINE-SYMBOL-MACRO)
     (MACRO-FUNCTION MACROEXPAND MACROEXPAND-1))

    ("Variable definition"
     (DEFPARAMETER DEFVAR DEFCONSTANT))

    ("Random number generator"
     RANDOM RANDOM-STATE-P MAKE-RANDOM-STATE)

    ("Strings"
     MAKE-STRING
     STRING
     ("Case conversion"
      (STRING-CAPITALIZE NSTRING-CAPITALIZE)
      (STRING-DOWNCASE NSTRING-DOWNCASE)
      (STRING-UPCASE NSTRING-UPCASE))
     ("Trimming"
      STRING-LEFT-TRIM
      STRING-RIGHT-TRIM
      STRING-TRIM)
     ("Comparison"
      ("Not case sensitive"
       STRING-EQUAL STRING-GREATERP STRING-LESSP
       STRING-NOT-EQUAL STRING-NOT-GREATERP STRING-NOT-LESSP)
      ("Case sensitive"
       STRING/= STRING< STRING<= STRING= STRING> STRING>=))
     ("Predicates"
      STRINGP SIMPLE-STRING-P)
     ("Accessor"
      CHAR SCHAR))

    ("No-op" (IDENTITY VALUES))

    ("Useful for :test and :key"
     IDENTITY COMPLEMENT CONSTANTLY)

    ("Multiple values"
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
     (EXPORT UNEXPORT IMPORT)
     IN-PACKAGE
     DEFPACKAGE
     (UNUSE-PACKAGE USE-PACKAGE PACKAGE-USE-LIST
                    PACKAGE-USED-BY-LIST)
     MAKE-PACKAGE
     PACKAGE-ERROR-PACKAGE
     PACKAGE-NAME
     PACKAGE-NICKNAMES
     PACKAGE-SHADOWING-SYMBOLS
     PACKAGEP
     (FIND-ALL-SYMBOLS FIND-PACKAGE FIND-SYMBOL)
     (SHADOW SHADOWING-IMPORT))

    ("Modules"
     (REQUIRE PROVIDE))

    ("Paths"
     TRUENAME
     ("Logical pathnames"
      TRANSLATE-LOGICAL-PATHNAME
      TRANSLATE-PATHNAME
      LOGICAL-PATHNAME
      LOGICAL-PATHNAME-TRANSLATIONS
      LOAD-LOGICAL-PATHNAME-TRANSLATIONS)
     USER-HOMEDIR-PATHNAME
     MERGE-PATHNAMES
     MAKE-PATHNAME
     (PARSE-NAMESTRING NAMESTRING ENOUGH-NAMESTRING)
     PATHNAME
     ("Components"
      PATHNAME-DEVICE PATHNAME-DIRECTORY PATHNAME-HOST
      PATHNAME-NAME PATHNAME-TYPE PATHNAME-VERSION)
     ("Predicates"
      PATHNAMEP WILD-PATHNAME-P PATHNAME-MATCH-P))

    ("Printing"
     ("Pretty printer"
      PPRINT
      ("Pprint dispatch"
       PPRINT-DISPATCH SET-PPRINT-DISPATCH COPY-PPRINT-DISPATCH)
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
     PRINT-UNREADABLE-OBJECT))))

(defparameter *equivalences*
  '((== (simple-string-p object)
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
  (walk *groups* nil))


#|

TODO add command for "finding similar functions or symbols"

|#
