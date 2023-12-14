
(cl:in-package :cl-user)

(defpackage #:breeze.test-file
  (:documentation "Parsing test files inspired by emacs' ERT's .erts files.")
  (:use #:cl)
  (:import-from #:alexandria
                #:symbolicate
                #:when-let
                #:make-keyword))

(in-package #:breeze.test-file)

(require 'alexandria)

(defun whitespacep (char)
  "Is CHAR a whitespace?"
  (position char #.(coerce '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return
                             #\Rubout)
                           'string)
            :test #'char=))

(defun trim-whitespace (string)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed
                 #\Page #\Return #\Rubout)
               string))

(defun string-bool (string)
  (cond
    ((string-equal string "nil") nil)
    ((string-equal string "t") t)
    (t string)))

(defun start-delimiter-p (string)
  (and string
       (string= string "=-=")))

(defun end-delimiter-p (string)
  (and string
       (string= string "=-=-=")))



(defmacro with-collectors ((&rest collectors) &body body)
  "Introduce a set of list with functions to push , get, set, etc those
lists."
  (let* ((variables (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
         (labels (loop :for collector :in collectors
                       :for v :in variables
                       :for push = (symbolicate 'push- collector)
                       :for set = (symbolicate 'set- collector)
                       :for drain = (symbolicate 'drain- collector)
                       :append `((,push (x)
                                        (unless (car ,v)
                                          (setf ,v nil))
                                        (let ((new-tail (cons x nil)))
                                          (if ,v
                                              (setf (cddr ,v) new-tail
                                                    (cdr ,v) new-tail)
                                              (setf ,v (cons new-tail new-tail))))
                                        x)
                                 (,set (&optional x)
                                       (unless ,v
                                         (setf ,v (cons nil nil)))
                                       (setf (car ,v) (copy-list x)
                                             (cdr ,v) (last (car ,v)))
                                       x)
                                 ((setf ,collector) (new-value) (,set new-value))
                                 (,drain () (,collector nil))
                                 (,collector (&optional (new-value nil new-value-p))
                                             (if new-value-p
                                                 (prog1 (when ,v (car ,v))
                                                   (,set new-value))
                                                 (when ,v (car ,v))))))))
    `(let ,variables
       (labels
           ,labels
         (declare (ignorable ,@(loop :for (label . rest) :in labels
                                     :collect `(function ,label))))
         ,@body))))

#++
(progn
  (with-collectors (x)
    (x '(32))
    (x))

  (with-collectors (x)
    (x '(32)))

  (with-collectors (x)
    (push-x 0)
    (push-x 1)
    (push-x 3)
    (x))

  (with-collectors (x y)
    (push-x 0)
    (push-y (copy-list (x)))
    (push-y 4)

    (push-x 1)
    (x '(a b c))
    ;; == (setf (x) '(a b c))
    ;; == (set-x '(a b c))

    (push-x 2)
    (push-x 3)

    (list (x) (y))
    ;; == (mapcar #'funcall (list #'x #'y))
    )
  ;; => ((A B C 2 3) ((0) 4))
  )

(defmacro with (clauses &body body)
  (loop
    :for clause :in (reverse clauses)
    :for (first . rest) = (if (listp clause)
                              clause
                              (list clause))
    :for symbol-package = (symbol-package first)
    :for symbol-name = (if (or
                            (eq 'with first)
                            (string= "COMMON-LISP"
                                     (package-name symbol-package)))
                           (symbol-name first)
                           (concatenate 'string "WITH-" (symbol-name first)))
    :do
       (multiple-value-bind (with status)
           (find-symbol symbol-name symbol-package)
         (cond
           ((null with)
            (error "Can't find symbol ~A:WITH-~A" (package-name symbol-package) symbol-name))
           ((eq 'with first)
            (setf body `((let ((,(first rest) ,@(when (rest rest)
                                                  `((with ,(rest rest))))))
                           ,@body))))
           ((and (not (eq *package* symbol-package)) (eq :internal status))
            (error "The symbol ~s is interal to ~s" with symbol-package))
           (t (setf body `((,with ,@rest ,@body)))))))
  (car body))

#++
(progn
  (with
      ((open-file (in "my-file")))
    test)

  (with
      ((output-to-string (out)))
    test)

  (with
      ((let ((y 42)))
       (with x (output-to-string (out)
                                 (format out "hello ~d" y))))
    x))



(defun read-spec-file (pathname)
  (with
      ((open-file (stream pathname))
       (collectors (tests parts))
       (let ((attributes (make-hash-table))
             (eof (gensym "eof"))))
       (macrolet
           ((push-char () `(write-char c out))))
       (labels
           ((peek (&optional (peek-type t))
              (peek-char peek-type stream nil eof))
            (get-char ()  (read-char stream))
            (eofp (x) (eq eof x))
            (clean-attributes () (remhash :skip attributes))
            (trim-last-newline (string)
              (let* ((end (1- (length string))))
                (if (char= #\Newline (char string end))
                    (subseq string 0 end)
                    string)))
            (read-comment (c)
              (when (char= #\; c)
                (read-line stream nil t)))
            (read-string (string)
              (loop :for c :across string
                    :do (char= c (get-char))))
            (read-test (c)
              (when (char= #\= c)
                (with-output-to-string (out)
                  (read-string #. (format nil "=-=~%"))
                  (loop :for line = (read-line stream)
                        :do (cond
                              ((start-delimiter-p line)
                               (push-parts (trim-last-newline (get-output-stream-string out))))
                              ((end-delimiter-p line)
                               (push-parts (trim-last-newline (get-output-stream-string out)))
                               (push-tests `(,@(alexandria:hash-table-plist attributes)
                                             :parts ,(drain-parts)))
                               (peek) ;; skip whitespaces
                               (clean-attributes)
                               (return-from read-test t))
                              ((string= "\\=-=" line)
                               (write-string line out :start 1)
                               (write-char #\newline out))
                              (t (write-string line out)
                                 (write-char #\newline out)))))))
            (read-attribute-name ()
              (make-keyword
               (string-upcase
                (with-output-to-string (out)
                  (loop :for c = (get-char)
                        :until (char= c #\:)
                        :do (write-char c out))))))
            (read-attribute-value ()
              (string-bool
               (trim-whitespace
                (with-output-to-string (out)
                  (loop
                    :for nl = nil :then (or (char= #\Linefeed c)
                                            (char= #\Return c))
                    :for c = (peek nil)
                    :until (or (eofp c)
                               (and nl (not (whitespacep c))))
                    :do
                       ;; (format t "~%c = ~s nl = ~s" c nl)
                       (if (read-comment c)
                           (unread-char (setf c #\Return) stream)
                           (write-char (get-char) out)))))))
            (read-attribute ()
              (let ((name (read-attribute-name))
                    (value (read-attribute-value)))
                (setf (gethash name attributes) value))))))
    (loop
      :for c = (peek)
      :repeat 250 ;; guard
      :until (eofp c)
      :for part = (or
                   (whitespacep c)
                   (read-comment c)
                   (read-test c)
                   (read-attribute))
      ;; :do (format t "~&~s" part)
      )
    ;; (format t "~&Final: ~% ~{~s~%~}" (tests))
    (tests)))

#++
(defparameter *structural-editing-tests*
  (read-spec-file
   (asdf:system-relative-pathname
    "breeze" "scratch-files/notes/strutural-editing.lisp")))


#++
(loop :for test :in *structural-editing-tests*
      :do (format t "~&~a: ~a parts"
                  (getf test :name)
                  (length (getf test :parts))
                  )
          ;; :do (print )
      )
