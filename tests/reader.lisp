
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

(defun read-from-string (string &optional (eof-error-p t)
                                  eof-value
                         &key
                           (start 0)
                           end
                           preserve-whitespace)
  "It's only useful for testing because it creates a new client everytime."
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
        suffix)
     &body body)
  "TODO Docstring
Introduce 5 lexical variables:
- input
- eof
- form
- position
- orphans"
  (flet ((suffix (symbol)
           (if suffix
               (symbolicate symbol suffix)
               symbol)))
    (let ((read-form `(read-from-string
                       input nil eof
                       :preserve-whitespace ,preserve-whitespace)))
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



;; TODO test (read-all-forms "...")

(define-test read-all-forms)

(define-test "1 symbol"
  :parent read-all-forms
  (is eq 'x
      (node-content
       (car (read-all-forms "x")))))


(define-test "WIP"
    (mapcar #'read-all-forms
            '(""
              "1"
              " 1"
              " 1 "
              "\"hi\""
              ";; hello"
              " ;; hello"
              "a ;; hello"
              "1 #|-|# \"x\" ")))



;;; Down below: "legacy mess" :P


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

;; TODO This is what needs fixing next:
#+nil
(unparse-to-string
 (parse-string " ( 2 ) 1 "))

#+nil
(parse-string " ( 2 ) 1 #| hey |# ")
#+nil
(let ((input " ( 2 ) 1 #| hey |# "))
  (loop :for node :in (parse-string input)
        :for (start . end) = (node-source node)
        :for last-child = ()
        ;; suffix would be (last child's end) - end
        :collect (list :subseq (subseq input start end)
                       :raw (node-raw node))))



#+nil
(parse-string "#|ads |#")
#+nil
(loop :for node :in (parse-string "1 #|-|# \"x\" ")
      :collect
      (type-of node)
      ;; (node-raw node)
      )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is all sort of broken right now
;;; I want to add a lot of more fundamental tests before re-enabling this
;;; I already created some tests for reading 1 thing from a string
;;; After thate I want to add tests on "read-all-forms"
;;; And finally add tests on post-processing and parse-string
;;; baby-steps
#+ (or)
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
             "#+(and) (bla)"))
    (let ((*break-on-signals* #+nil 'error))
      (let* ((nodes (parse-string expected))
             (got (unparse-to-string nodes)))
        (is string= expected got
            "nodes: ~a" nodes)))))

;; TODO (apply #'concatenate 'string (list node)) should= input
;; TODO There shouldn't be any gaps in the node-source

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


#|
This is a valid syntax:
#p
#+smth "..."
#-smth2 "..."
|#


#|
What about nested feature expressions?
|#



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

This one can only be inserted at the end
; comment                               ; ; ; ; ;

'()
#()

|#
