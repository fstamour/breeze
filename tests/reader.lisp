;;;; TODO Take a look at the relevant ANSI tests: https://gitlab.common-lisp.net/ansi-test/ansi-test/-/tree/master/reader
;;;; TODO Take a look at those reader tests: https://github.com/informatimago/lisp/blob/4bfb6893e7840b748648b749b22078f2facfee0a/common-lisp/lisp-reader/reader-test.lisp

(cl:in-package #:common-lisp-user)

(defpackage #:breeze.test.reader
  (:use :cl #:breeze.reader)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false)
  (:import-from #:breeze.reader
                #:breeze-client
                #:read-all-forms)
  (:shadow #:read-from-string))

(in-package #:breeze.test.reader)


;;; Testing eclector.parse-result:READ-FROM-STRING
;;;
;;; This is important because it the building block of the rest of the
;;; reader (a.k.a READ-ALL-FORMS)

(defun read-from-string (string &optional (eof-error-p t)
                                  eof-value
                         &key
                           (start 0)
                           end
                           preserve-whitespace)
  "It's not  useful outside of testing because it creates a new
client everytime it is invoked."
  (eclector.parse-result:read-from-string
   (make-instance 'breeze-client
                  :source string)
   string
   eof-error-p
   eof-value
   :start start
   :end end
   :preserve-whitespace preserve-whitespace))

(defmacro with-read-from-string
    ((input
      &key
        preserve-whitespace
        (start 0)
        suffix)
     &body body)
  "This is a macro to make it easier to test the behaviour of eclector
's read-from-string function.

Introduce 5 lexical variables:
- input: the input string
- eof: the symbol return when the reader reaches the end of the string
- form: the syntax-node returned
- position: the new position of the reader
- orphans: syntax-nodes for skipped-inputs
"
  (flet ((suffix (symbol)
           (if suffix
               (symbolicate symbol suffix)
               symbol)))
    (let ((read-form `(read-from-string
                       input nil eof
                       :preserve-whitespace ,preserve-whitespace
                       :start ,start
                       )))
      `(let ((,(suffix 'input) ,input)
             ;; yes, gensym at run-time, it's not a mistake
             (,(suffix 'eof) (gensym "eof")))
         ,(if body
              `(multiple-value-bind
                     (,(suffix 'form)
                      ,(suffix 'position)
                      ,(suffix 'orphans))
                   ,read-form
                 ,@body)
              read-form)))))

#+ (or) ;; WIP
(defmacro read= ((input1 &option preserve-whitespace1)
                 (input2 &option preserve-whitespace2))
  `(with-read-from-string (,input1
                           :preserve-whitespace ,preserve-whitespace1
                           :suffix 1)
     (with-read-from-string (,input2
                             :preserve-whitespace ,preserve-whitespace2
                             :suffix 2)
       (is eq (type-of form1) (type-of form2)))))

(define-test "read empty string"
  ;;; Empty string
  (with-read-from-string ("")
    (is eq eof form)
    (is = 0 position)
    (false orphans))
  (with-read-from-string ("" :preserve-whitespace t)
    (is eq eof form)
    (is = 0 position)
    (false orphans)))

(define-test "read only whitespaces"
  (with-read-from-string (" ")
    (is eq eof form)
    (is = 1 position)
    (false orphans))
  (with-read-from-string (" " :preserve-whitespace t)
    (is eq eof form)
    (is = 1 position)
    (false orphans)))

(define-test "read symbol"
  (with-read-from-string ("x ")
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (destructuring-bind (start . end)
        (node-source form)
      (is = 0 start)
      (is = 1 end))
    (is = 2 position)
    (false orphans))
  (with-read-from-string ("x " :preserve-whitespace t)
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (destructuring-bind (start . end)
        (node-source form)
      (is = 0 start)
      (is = 1 end))
    (is = 1 position)
    (false orphans)))

(define-test "read symbol with offset"
  (with-read-from-string ("012345x" :start 6)
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (destructuring-bind (start . end)
        (node-source form)
      ;; IMO this should be 6, but that's not how eclector works
      (is = 0 start)
      ;; IMO this should be 7
      (is = 1 end))
    (is = 7 position)
    (false orphans)))

(define-test "read string"
  (with-read-from-string (" \"s\" ")
    (is eq 'string-node (type-of form))
    (is string= "s" (node-content form))
    (destructuring-bind (start . end)
        (node-source form)
      (is = 1 start)
      (is = 4 end))
    (is = position (length input))
    (false orphans))
  (with-read-from-string (" \"s\" " :preserve-whitespace t)
    (is eq 'string-node (type-of form))
    (is string= "s" (node-content form))
    (destructuring-bind (start . end)
        (node-source form)
      (is = 1 start)
      (is = 4 end))
    (is = position (1- (length input)))
    (false orphans)))

(define-test "read line comment"
  (with-read-from-string (" ;; ")
    (is eq eof form)
    (is = position (length input))
    (is = 1 (length orphans))
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= ";; " (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 1 start)
        (is = 4 end))))
  (with-read-from-string (" ;; " :preserve-whitespace t)
    (is eq eof form)
    (is = position (length input))
    (is = 1 (length orphans))
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= ";; " (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 1 start)
        (is = 4 end)))))

;; I keep learning new tricks with loop...
#+ (or)
(loop :for i :below 10
      :if (oddp i)
        :append (list i i)
      :else
        :collect i)

;; WIP
(define-test "read symbol followed by line comment"
  (with-read-from-string (" x ;; ")
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (is = 3 position
        "where does the reader stop in ~s?" input)
    (is = 0 (length orphans)))
  (with-read-from-string (" x ;; " :preserve-whitespace t)
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (is = 2 position
        "where does the reader stop in ~s?" input)
    (is = 0 (length orphans))))


(define-test "read block comment"
;;; Block comment
  (with-read-from-string ("#||#")
    (is eq eof form)
    (is = position (length input))
    (is = 1 (length orphans))
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#||#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 0 start)
        (is = 4 end))))
  (with-read-from-string ("#||#" :preserve-whitespace t)
    (is eq eof form)
    (is = position (length input))
    (is = 1 (length orphans))
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#||#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 0 start)
        (is = 4 end))))
;;; Block comment with spaces around
  (with-read-from-string ("#||# ")
    (is eq eof form)
    (is = 5 position)
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#||#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 0 start)
        (is = 4 end))))
  (with-read-from-string ("#||# " :preserve-whitespace t)
    (is eq eof form)
    ;; See how :preserve-whitespace changed nothing here?
    (is = 5 position)
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#||#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 0 start)
        (is = 4 end))))
  (with-read-from-string (" #||#")
    (is eq eof form)
    (is = 5 position)
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#||#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 1 start)
        (is = 5 end))))
  (with-read-from-string (" #||#" :preserve-whitespace t)
    (is eq eof form)
    (is = 5 position)
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#||#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 1 start)
        (is = 5 end))))
  (with-read-from-string (" #||# ")
    (is eq eof form)
    (is = 6 position)
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#||#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 1 start)
        (is = 5 end))))
  (with-read-from-string (" #||# " :preserve-whitespace t)
    (is eq eof form)
    (is = 6 position)
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#||#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 1 start)
        (is = 5 end)))))

(define-test "read symbol followed by block comment"
  (with-read-from-string ("x #| |#")
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (is = 2 position)
    (is = 0 (length orphans)))
  (with-read-from-string ("x #| |#" :preserve-whitespace t)
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (is = 1 position)
    (is = 0 (length orphans))))

(define-test "read block comment comment followed by symbol"
  (with-read-from-string ("#| |# x")
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (is = 1 (length orphans))
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#| |#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 0 start)
        (is = 5 end))))
  (with-read-from-string ("#| |# x" :preserve-whitespace t)
    (is eq 'symbol-node (type-of form))
    (is eq 'x (node-content form))
    (is = 1 (length orphans))
    (let ((node (car orphans)))
      (is eq 'skipped-node (type-of node))
      (is string= "#| |#" (node-content node))
      (destructuring-bind (start . end)
          (node-source node)
        (is = 0 start)
        (is = 5 end)))))

;;;
;;; TODO define-test "read block comment surrounded by symbols"
;;; TODO define-test "read nested block comments"
;;; TODO define-test "read multiple block comments" (one after the other)
;;;

(define-test "read int"
  (with-read-from-string ("1")
    (is eq 'node (type-of form))
    (is = 1 (node-content form))))

;; TODO
#+ (or)
(define-test "read complex"
  (with-read-from-string ("#C(1 2)")))


#|
This is a valid syntax:
#p
#+smth "..."
#-smth2 "..."
|#


#|
What about nested feature expressions?
|#


;;; TODO Test package-local-nicknames


(define-test in-package
  (let ((in-package-form (first (parse-string "(in-package :cl)"))))
    (true (in-package-form-p in-package-form))
    (is eq :cl (in-package-node-package in-package-form))))


;;; Test READ-ALL-FORMS

;; TODO test (read-all-forms "...")

(define-test read-all-forms)

(define-test "read-all-forms 1 symbol"
  :parent read-all-forms
  (is eq 'x
      (node-content
       (car (read-all-forms "x")))))


;; TODO recurse into non-terminal nodes
(defun contiguousp (nodes input)
  (loop :for node :in nodes
        :for next-node :in (cdr nodes)
        :always
        (or (null next-node)
            (is =
                (1+ (node-end node))
                (node-start next-node)
                "The node ~a ends at ~a and the next-node ~a starts at ~a in the input ~s"
                node (node-end node)
                next-node (node-start next-node)
                input))))

(define-test "nodes are contiguous"
  (loop :for input :in '(""
                         "1"
                         " 1"
                         " 1 "
                         " a b c "
                         "\"hi\""
                         ";; hello"
                         " ;; hello"
                         "a ;; hello"
                         "1 #|-|# \"x\" "
                         "( a b c ) d")
        :for nodes = (read-all-forms input)
        :do (true (contiguousp nodes input)
                  "~s is not contiguous" input)))

;; DONE read-all-forms' output should be contiguous (the end of one
;; form should be = to the start of the next form)
;; TODO it should span the whole input.
;; TODO there should be no overlap
;; TODO There shouldn't be any gaps in the node-source
;;
;; ^^^ these are redundant but I want to be extra sure


;;; Test POST-PROCESS-NODES!


;;; Test PARSE-STRING

;; TODO Turn this into tests
;; TODO There should be equivalent tests for read-all-forms
#+nil
(loop :for node :in (parse-string "1 #|-|# \"x\" ")
      :collect
      (type-of node)
      ;; (node-raw node)
      )
;; (NODE SKIPPED-NODE STRING-NODE SKIPPED-NODE)


(defun test-node (node type prefix content raw)
  (true (typep node 'node)
        "~a should be of type node" node)
  (true (typep node type)
        "~a should be of type ~a" node type)
  (if prefix
      (is string= prefix (node-prefix node)
          "~a should have a prefix ~a" node (node-prefix node))
      (false (node-prefix node)
             "~a should not have a node prefix" node))
  (is equalp content (node-content node))
  (is string= raw (node-raw node)))

(defun test-node* (nodes spec-list)
  (mapcar #'(lambda (node spec)
              (apply #'test-node node spec))
          nodes
          spec-list))


;; TODO These are now almost all obsolete
;; specs: type prefix content raw
#+ (or)
(define-test parse-string
  (false (parse-string ""))
  (test-node* (parse-string "1")
              '((node nil 1 "1")))
  (test-node* (parse-string " 1")
              '((node " " 1 " 1")))
  (test-node* (parse-string " 1 ")
              '((node " " 1 " 1")
                (skipped-node nil " " nil)))
  (test-node* (parse-string "\"hi\"")
              '((node nil "hi" "\"hi\"")))
  (test-node* (parse-string ";; hello")
              '((skipped-node nil ";; hello" nil)))
  (test-node* (parse-string " ;; hello")
              '((skipped-node nil " ;; hello" nil)))
  (test-node* (parse-string "a ;; hello")
              '((symbol-node nil a "a")
                (skipped-node nil " ;; hello" nil)))
  (test-node* (parse-string "1 #|-|# \"x\" ")
              '((symbol-node nil 1 "1")
                (skipped-node " #|-|#" nil nil)
                (string-node " " "x" " \"x\"")
                (skipped-node nil " " nil))))


;;; Test UNPARSE-TO-STRING

;; TODO This should _in theory_ easy to tests, but it's kind of hard
;; to actually create nodes without parse-string


;;; Test "roundtrip" (parse followed by unparse

(define-test "roundtrip list and numbers"
  (unparse-to-string
   (parse-string " ( 2 ) 1 ")))


;;; Down below: "legacy mess" :P

(define-test parse-unparse-roundtrip
  (dolist (expected
           '("1"
             " 1 "
             "()"
             " ( 2 )"
             "\"hi\""
             ";; hello"
             " #| hello |#"
             "a ;; hello"
             "b  #| hello |#"
             "(1 #|comment|# \"string\")"
             "`(,a ,b)"
             "(1 . 2)"
             "#.(+ 1 2)"
             "#+nil ingored"
             "#\\Space"
             "a"
             "A"
             " nil "
             " NiL "
             " () "
             " ( ) "
             " ( a ) "
             " ( a b  ) "
             " '() "
             "(((a)))"
             "(quote a b c)"
             "'a"
             "`a"
             "`(,a)"
             "`(,@a)"
             "()"
             "#.()"
             "#.(+ 1 2)"
             "#'print"
             "#-(or) 1"
             "#+nil 1 "
             "#+(and) 2"
             ;; "#+(and) (bla)"
             ))
    (let ((*break-on-signals* #+nil 'error))
      (let* ((nodes (parse-string expected))
             (got (unparse-to-string nodes)))
        ;; (print expected) (force-output)
        (is string= expected got
            "nodes: ~a" nodes)))))





;;; Reading source files from this project

;; TODO test round-tripping of all files in this project!

#+ (or)
(progn
  (eclector.reader:read-from-string "#+ (or) asdf")

  (let ((eclector.reader:*client*))
    (eclector.reader:read-from-string "#+(or) t"))

  (parse-string "#+ (and) t")

  ;; Trying to read all files in a system, using breeze's reader.
  (time
   (loop :for file :in (breeze.asdf:system-files 'breeze)
         :for content = (alexandria:read-file-into-string file)
         :do
            (with-open-file (stream file)
              (let* ((nodes (parse stream))
                     #+nil
                     (unparse-content (unparse-to-string nodes)))
                #+nil
                (unless (string= content unparse-content)
                  (format t "~&Failed to roundtrip file ~s." file))))
            (format t "~&~s parsed." file))))


#+ (or)
(parse-string
 (alexandria:read-file-into-string #P"..../breeze/src/utils.lisp"))

#+ (or)
(breeze.asdf:system-files 'breeze)





;;; Drafting some generative tests

#|
Each of these can be inserted anywhere
x
1
#C(2 3)
"asdf"
#||#

#'f
'x
'()

This one can only be inserted at the end (of a line)
                                        ; comment                               ; ; ; ; ; ; ; ; ; ; ;

'()
#()

|#

;; TODO (apply #'concatenate 'string (list node)) should= input
