;;;; Trying to create some kind of "architectural" tests/helpers.
;;;; To make refactorings easier.
;;;;
;;;; I think this might belong into breeze instead

(in-package #:cl-user)

(ql:quickload '(#:alexandria
                #:anaphora
                #:cl-arrows
                #:fset
                #:split-sequence))

(uiop:define-package #:arch
  (:use #:cl #:alexandria #:anaphora #:cl-arrows))

(in-package #:arch)

(defun read-funcall-loop (stream fn)
  "Call fn on each form read from a stream."
  (let ((eof (gensym "eof")))
    (loop :for form = (read stream nil eof)
          :until (eq eof form)
          :collect (funcall fn form))))

(defun read-forms (stream)
  (read-funcall-loop stream #'identity))

(defun get-defsystem (path)
  "Find the first top-level form that is a list that starts with the symbol 'defsystem."
  (find-if #'(lambda (form) (eq 'defsystem (first form)))
           (with-open-file (file path)
             (read-forms file))))

(defun flatten-system-component (component-list &optional prefix)
  (loop :for component :in component-list
        :append
        (ecase (first component)
          (:module (flatten-system-component
                    (getf component :components)
                    `(,@prefix ,(second component))))
          (:file (list `(,@prefix ,(second component)))))))

#+test
((flatten-system-component ())
 (flatten-system-component '((:file main)))
 (flatten-system-component '((:file package)
                             (:file main)))
 (flatten-system-component '((:module src
                              :components
                              ((:file package)
                               (:file main))))))

(defun each (function)
  "Defered mapcar."
  (lambda (list)
    (mapcar function list)))

(defun lisp-file-p (pathname)
  "Return true if pathname designate a lisp file."
  (and
   (not (starts-with-subseq "." (pathname-name pathname)))
   (string-equal "lisp" (pathname-type pathname))))

(defun lisp-file-in-directory (directory)
  "Return the list of lisp file in a directory."
  (remove-if-not
   #'lisp-file-p
   (uiop:directory-files (truename directory))))

(defun pathname-to-relative-list-of-symbol (pathname)
  (mapcar (compose #'intern #'string-upcase)
          (let ((enough (enough-namestring pathname)))
            `(,@(rest (pathname-directory enough)) ,(pathname-name enough)))))

(defun find-missing-file-in-system (system-pathname directory-name)
  "This is very opiniated."
  (let ((component-list (flatten-system-component
                         (getf
                          (get-defsystem system-pathname)
                          :components))))
    (uiop:while-collecting (collect)
      (uiop/filesystem:collect-sub*directories
       directory-name
       ;; collectp
       (constantly t)
       ;; recursep
       (constantly t)
       ;; collector
       #'(lambda (directory)
           (awhen
            (remove-if #'(lambda (component)
                           (member component component-list :test #'equal))
                       (mapcar #'pathname-to-relative-list-of-symbol
                               (lisp-file-in-directory directory)))
             (collect it)))))))

#+nil
;; Get the list of files that are in redmoon.asd but aren't in "src/"
((find-missing-file-in-system "redmoon.asd" "src")
 ;; => ((SRC BASIC-CODE-MANIP))

 (find-missing-file-in-system "redmoon.test.asd" "tests")
 ;; => (((TESTS BASIC-CODE-MANIP.TEST)) NIL)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (uiop:while-collecting (a b c) (a 1) (b 2) (c 3))

(defun symbol-list-alist-gensym (symbol-list)
  "Take a list of symbol, returns an alist of gensyms.
e.g. (a b c) => ((a . #:a738) (b . #:b739) (c . #:c740))"
  (loop :for collector :in symbol-list
        :collect `(,collector . ,(gensym (symbol-name collector)))))

#+nil
(cdr
 (assoc 'a
        (symbol-list-alist-gensym '(a b c))))

(defmacro while-collecting-unique ((&rest collectors) &body body)
  (with-gensyms (x)
    (let ((collector-gensym (symbol-list-alist-gensym collectors)))
      (flet ((get-gensym (collector) (cdr (assoc collector collector-gensym))))
        `(let (,@(loop :for c :in collectors
                       :collect `(,(get-gensym c) (fset:set))))
           (flet (,@(loop :for c :in collectors
                          :for g = (get-gensym c)
                          :collect `(,c (,x)
                                        (setf ,g (fset:with ,g ,x)))))
             ,@body
             (values ,@(mapcar #'cdr collector-gensym))))))))

#+nil
(macroexpand-1
 '(while-collecting-unique (a b c) some body))

#+nil
(while-collecting-unique (add)
  (mapcar #'add '(a a b c a b c c b a)))
;; => #{ a b c }

(defun extract-package (pathname)
  "Extract all the \"in-package\" from a file."
  (mapcar #'second
          (remove-if-not (compose (curry #'eq 'in-package) #'first)
                         (with-open-file (file pathname)
                           (read-forms file)))))

(defun sanitize-list-list-symbol (lls)
  "Takes a list of list of symbol, returns a flat list of unique symbol's name."
  (remove-duplicates
   (sort
    (mapcar #'symbol-name (reduce #'append lls)) #'string<)
   :test #'string=))

(defun extract-package-directory (directory-name)
  "Go through every lisp files in a directory and returns the list of packages
used in (in-package) forms."
  (sanitize-list-list-symbol
   (uiop:while-collecting (collect)
     (uiop/filesystem:collect-sub*directories
      directory-name
      ;; collectp
      (constantly t)
      ;; recursep
      (constantly t)
      ;; collector
      #'(lambda (directory)
          (mapcar #'collect
                  (mapcar #'extract-package
                          (lisp-file-in-directory directory))))))))
#+nil
((extract-package-directory "src")
  ;; =>
 ("CL-USER" "REDMOON" "REDMOON.CONTEXT" "REDMOON.CORE.MACROS" "REDMOON.EVAL"
            "REDMOON.TYPE" "REDMOON.UTILS")

 (extract-package-directory "tests")
 ;; =>
 ("CL-USER" "PARACHUTE" "REDMOON.CONTEXT.TEST" "REDMOON.CORE.MACROS.TEST"
            "REDMOON.CORE.TEST" "REDMOON.EVAL.TEST" "REDMOON.TEST" "REDMOON.TYPE.TEST"))

(defun filter-redmoon-package (package-list)
  (remove-if-not #'(lambda (string)
                     (search "REDMOON" string :test #'string-equal))
                 package-list))

;; Find the test packages that match the pattern "redmoon.TEST.smth" (instead of "readmoon.smth.TEST")
#+nil
(loop :for package :in
                   (filter-redmoon-package
                    '("CL-USER" "PARACHUTE" "REDMOON.CONTEXT.TEST" "REDMOON.CORE.MACROS.TEST"
                      "REDMOON.CORE.TEST" "REDMOON.EVAL.TEST" "REDMOON.TEST" "REDMOON.TYPE.TEST"))
      :for parts = (split-sequence:split-sequence #\. package)
      :when (and
             (> (length parts) 2)
             (string= "TEST" (second parts)))
      :collect parts)

;; trying to find disprecancies between the packages and test packages


(defun reverser-parameter (fn)
  "Take a function of arity 2 and call return a function with the 2
 parameters inverted."
  #'(lambda (x y) (funcall fn y x)))

#+nil
;; This is the logic we want for this.
(flet ((test (source test)
         (equalp source
                 (remove 't test))))
  (let ((x '((a b) (a c)))
        (y '((a b t) (a d t))))
    (list
     (set-difference x y :test #'test)
     (set-difference y x :test (reverser-parameter #'test)))))

;; It's kind of neat, but the name need some reworking :P
#+nil ((flet ((foo (directory-name)
                (mapcar (curry #'split-sequence:split-sequence #\.)
                        (filter-redmoon-package (extract-package-directory directory-name))))
              (test (source test)
                (equalp source
                        (remove "test" test :test #'string-equal))))
         (let ((x (foo "src"))
               (y (foo "tests")))
           (list
            (set-difference x y :test #'test)
            (set-difference y x :test (reverser-parameter #'test)))))

;; =>
       ((("REDMOON" "UTILS")) (("REDMOON" "CORE" "TEST"))))
