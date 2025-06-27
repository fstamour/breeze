;; TODO ironically... there's no tests for this code...

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


;;; Classes

(defclass test-spec ()
  ((name
    :initform nil
    :initarg :name
    :accessor name
    :documentation "The name of the test.")
   (code
    :initform nil
    :initarg :code
    :accessor code
    :documentation "A string that specifies what to run for this test.")
   (point-char
    :initform nil
    :initarg :point-char
    :accessor point-char
    :documentation "The character that represents the point in the parts.")
   (before
    :initform nil
    :initarg :before
    :accessor before
    :documentation "A string representing the input of the test.")
   (after
    :initform nil
    :initarg :after
    :accessor after
    :documentation "A string representing the expected output of the test.")
  (skipp
    :initform nil
    :initarg :skipp
    :accessor skipp
    :documentation "Whether this test should be skipped.")
   (source-pathname
    :initform nil
    :initarg :source-pathname
    :accessor source-pathname
    :documentation "The pathname of the file from which this specification was read from.")
   (source-position
    :initform nil
    :initarg :source-position
    :accessor source-position
    :documentation "A cons (START . END) that corresponds to the file-position of this
specification in the file it was read from.")
   ;; TODO perhaps "source": which file it was read from, which position in the file?
   )
  (:documentation "A test that was specified in an \"erts\" file."))

(defmethod print-object ((test-spec test-spec) stream)
  (print-unreadable-object
      (test-spec stream :type t :identity nil)
    (destructuring-bind (start . end)
        (source-position test-spec)
      (format stream "~s ~s-~s" (name test-spec) start end))))


;;; Parsing utils

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


;;; Low-level ERTS file parsing

(defun %read-spec-file (pathname)
  (with
      ((open-file (stream pathname))
       (collectors (tests parts))
       (let ((attributes (make-hash-table))
             (eof '#.(gensym "eof"))
             (start 0)))
       (labels
           ((peek (&optional (peek-type t))
              (peek-char peek-type stream nil eof))
            (get-char () (read-char stream))
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
              ;; TODO this doesn't actually check anything...
              (loop :for c :across string
                    :do (char= c (get-char))))
            (read-test (c)
              (when (char= #\= c)
                (with-output-to-string (out)
                  (read-string #.(format nil "=-=~%"))
                  (loop :for line = (read-line stream)
                        :do (cond
                              ((part-delimiter-p line)
                               (push-parts (trim-last-newline
                                            (get-output-stream-string out))))
                              ((end-delimiter-p line)
                               (push-parts (trim-last-newline
                                            (get-output-stream-string out)))
                               (push-tests `(,@(alexandria:hash-table-alist attributes)
                                               (:parts ,@(drain-parts))
                                               (:pathname ,@pathname)
                                               (:position ,@(cons start
                                                                 (file-position stream)))))
                               (peek) ;; skip whitespaces
                               (setf start (file-position stream))
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
      ;; :repeat 250 ;; guard
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


;;; ERTS file parsing and validation

;; TODO test-spec-condition instead of generic errors
(defun read-spec-file (pathname)
  ;; N.B. the validatoin would almost never fail, because the
  ;; attributes are carried over, if a test doesn't specify a required
  ;; attribute, it'll just "inherit" the attribute's value from the
  ;; preceding test.  I'm keeping this check in case there's a bug in
  ;; the parser.
  (let ((specs (%read-spec-file pathname)))
    (flet ((get-attribute (spec key)
             (cdr (assoc key spec))))
      (loop :for test :in specs
            :for pathname = (get-attribute test :pathname)
            :for position = (get-attribute test :position)
            :for (start . end) = position
            :for name = (get-attribute test :name)
            :unless name
              :do (error "A test is missing a name (~s between char ~d and ~d)"
                         pathname start end)
            :collect
            (let ((code (get-attribute test :code))
                  (point-char (get-attribute test :point-char))
                  (parts (get-attribute test :parts))
                  (skipp (get-attribute test :skip))
                  before after)
              (unless code
                (error "The test ~s is missing the \"Code:\" attribute." name))
              (unless point-char
                (error "The test ~s is missing the \"Point-Char:\" attribute." name))
              (unless parts
                (error "The test ~s is missing the actual test input(s)." name))
              (let ((number-of-parts (length parts)))
                (case number-of-parts
                  (1
                   (setf before (first parts)
                         after (first parts)))
                  (2 (setf before (first parts)
                           after (second parts)))
                  (t
                   (unless (<= 1 number-of-parts 2)
                     (error "The test ~s (defined in ~s between the chars ~d and ~d) has ~d parts, a test should have only 1 or 2."
                            name pathname start end number-of-parts)))))
              ;; TODO Don't discard the "unknown" attributes The way
              ;; the parser is written, there's no limitation on the
              ;; valid attributes. Let's say that these here are the
              ;; "well-known" attributes.
              (make-instance 'test-spec
                             :name name
                             :source-pathname pathname
                             :source-position position
                             :code code
                             :point-char point-char
                             :before before
                             :after after
                             :skipp skipp))))))

#++
(defparameter *structural-editing-tests*
  (read-spec-file
   (asdf:system-relative-pathname
    "breeze" "scratch-files/notes/strutural-editing.lisp")))

;; parachute:define-test
;; parachute::ensure-test

#++
(loop :for test :in *structural-editing-tests*
      :do )
