
(cl:in-package :cl-user)

(defpackage #:breeze.test-file
  (:documentation "Parsing test files inspired by emacs' ERT's .erts files.")
  (:use #:cl)
  (:import-from #:alexandria
                #:symbolicate
                #:when-let
                #:make-keyword)
  (:import-from #:breeze.string
                #:whitespacep
                #:trim-whitespace)
  (:import-from #:breeze.utils
                #:with-collectors
                #:with)
  (:export #:read-spec-file))

(in-package #:breeze.test-file)

(defun string-bool (string)
  "If string is a representation of T or NIL, then coerce it."
  (cond
    ((string-equal string "nil") nil)
    ((string-equal string "t") t)
    (t string)))

(defun part-delimiter-p (string)
  (and string
       (string= (trim-whitespace string) "=-=")))

(defun end-delimiter-p (string)
  (and string
       (string= (trim-whitespace string) "=-=-=")))



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
                              ((part-delimiter-p line)
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
                  (length (getf test :parts)))
          ;; :do (print )
      )
