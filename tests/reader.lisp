
(cl:in-package #:common-lisp-user)

(defpackage #:breeze.test.reader
  (:use :cl #:breeze.reader)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false))

(in-package #:breeze.test.reader)

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
#+ (or)
(car
 (parse-string " ( 2 ) "))

(define-test parse-string
  (false (parse-string ""))
  (test-node* (parse-string "1")
              '((node nil 1 "1")))
  (test-node* (parse-string " 1")
              '((node " " 1 "1")))
  (test-node* (parse-string " 1 ")
              '((node " " 1 "1")
                (skipped-node nil " " nil)))
  (test-node* (parse-string "\"hi\"")
              '((node nil "hi" "\"hi\"")))
  (test-node* (parse-string ";; hello")
              '((skipped-node nil ";; hello" nil)))
  (test-node* (parse-string " ;; hello")
              '((skipped-node nil " ;; hello" nil)))
  (test-node* (parse-string "a ;; hello")
              '((symbol-node nil a "a")
                (skipped-node nil " ;; hello" nil))))

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
