
(defpackage #:breeze.utils
  (:use :cl)
  (:documentation "Utilities")
  (:export
   #:around
   #:package-apropos
   #:optimal-string-alignment-distance
   #:optimal-string-alignment-distance*
   #:walk
   #:walk-car
   #:walk-list
   #:indent-string
   #:remove-indentation
   #:print-comparison
   #:summarize
   #:breeze-relative-pathname
   #:+whitespaces+
   #:whitespacep
   #:stream-size
   #:read-stream-range
   #:symbol-package-qualified-name
   #:before-last
   #:find-version-control-root
   #:find-asdf-in-parent-directories
   #:subseq-displaced
   #:length>1?))

(in-package #:breeze.utils)


;;; Other

;; TODO I don't think I use this
(defun walk (tree fn  &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on every elements"
  (dolist (node tree)
    (if (listp node)
        (when (funcall recurse-p)
          (walk node fn recurse-p))
        (funcall fn node))))

;; TODO I don't think I use this
(defun walk-list (tree fn &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on each list parts"
  (when (and (listp tree) (listp (cdr tree)))
    (funcall fn tree)
    (dolist (node tree)
      (when (funcall recurse-p node)
        (walk-list node fn recurse-p)))))

;; TODO I don't think I use this
(defun walk-car (tree fn &optional (recurse-p (constantly t)))
  "Walk a tree and call fn on each first elements (cars)"
  (walk-list tree
             #'(lambda (node)
                 (funcall fn (car node)))
             recurse-p))

(defun package-apropos (search-string)
  "Compute a list of package that contains the search-string."
  (remove-if-not #'(lambda (package)
                     (search search-string (package-name package)
                             :test #'string-equal))
                 (list-all-packages)))


;;; String stuff

(defun optimal-string-alignment-distance (vec-a vec-b)
  "Compute an edit distance between two vector."
  (let* ((m (length vec-a))
         (n (length vec-b))
         (diff-0 (make-array (list (1+ n)) :element-type 'integer))
         (diff-1 (make-array (list (1+ n)) :element-type 'integer))
         (diff-2 (make-array (list (1+ n)) :element-type 'integer)))

    (loop :for i :upto n :do
      (setf (aref diff-1 i) i))
    (setf (aref diff-0 0) 1)

    (flet ((a (index) (aref vec-a (1- index)))
           (b (index) (aref vec-b (1- index)))
           (diff-0 (index) (aref diff-0 index))
           (diff-1 (index) (aref diff-1 index))
           (diff-2 (index) (aref diff-2 index)))
      (loop :for i :from 1 :upto m :do
        (loop :for j :from 1 :upto n
              :for cost = (if (eq (a i) (b j)) 0 1) ;; aka substitution-cost
              :do
                 (setf (aref diff-0 j) (min
                                        (1+ (diff-1 j))          ;; deletion
                                        (1+ (diff-0 (1- j)))     ;; insertion
                                        (+ cost (diff-1 (1- j))) ;; substitution
                                        ))
                 ;; transposition
                 (when (and (< 1 i) (< 1 j)
                            (eq (a i) (b (1- j)))
                            (eq (a (1- i)) (b j)))
                   (setf (aref diff-0 j) (min (diff-0 j)
                                              (+ cost (diff-2 (- j 2)))))))
        (when (/= m i)
          (let ((tmp diff-2))
            (setf diff-2 diff-1
                  diff-1 diff-0
                  diff-0 tmp
                  (aref diff-0 0) (1+ i)))))
      (diff-0 n))))

(defun optimal-string-alignment-distance* (vec-a vec-b max-distance)
  "Compute an edit distance between two vector. Stops as soon as
max-distance is reached, returns nil in that case."
  (unless (> (abs (- (length vec-a)
                     (length vec-b)))
             max-distance)
    (let* ((m (length vec-a))
           (n (length vec-b))
           (diff-0 (make-array (list (1+ n)) :element-type 'integer))
           (diff-1 (make-array (list (1+ n)) :element-type 'integer))
           (diff-2 (make-array (list (1+ n)) :element-type 'integer)))

      (loop :for i :upto n :do
        (setf (aref diff-1 i) i))
      (setf (aref diff-0 0) 1)

      (flet ((a (index) (aref vec-a (1- index)))
             (b (index) (aref vec-b (1- index)))
             (diff-0 (index) (aref diff-0 index))
             (diff-1 (index) (aref diff-1 index))
             (diff-2 (index) (aref diff-2 index)))
        (loop
          :for min-distance = nil
          :for i :from 1 :upto m :do
            (loop :for j :from 1 :upto n
                  ;; aka substitution-cost
                  :for cost = (if (eq (a i) (b j)) 0 1)
                  :do
                     (setf (aref diff-0 j) (min
                                            ;; deletion
                                            (1+ (diff-1 j))
                                            ;; insertion
                                            (1+ (diff-0 (1- j)))
                                            ;; substitution
                                            (+ cost (diff-1 (1- j)))))
                     ;; transposition
                     (when (and (< 1 i) (< 1 j)
                                (eq (a i) (b (1- j)))
                                (eq (a (1- i)) (b j)))
                       (setf (aref diff-0 j) (min (diff-0 j)
                                                  (+ cost (diff-2 (- j 2))))))
                     (when (or (null min-distance)
                               (> min-distance (diff-0 j)))
                       ;; (format *debug-io* "~&new min-distance ~s" min-distance)
                       (setf min-distance (diff-0 j))))
            ;; (format *debug-io* "~&~s ~s" i diff-0)
            (when (and (> i 1)
                       (>= min-distance max-distance))
              #+ (or)
              (format *debug-io* "~&min-distance ~s > max-distance ~s"
                      min-distance max-distance)
              (return-from optimal-string-alignment-distance*))
            (when (/= m i)
              (let ((tmp diff-2))
                (setf diff-2 diff-1
                      diff-1 diff-0
                      diff-0 tmp
                      (aref diff-0 0) (1+ i)))))
        (diff-0 n)))))

(defun indent-string (indentation string)
  "Prepend INDENTATION spaces at the beginning of each line in STRING."
  (check-type indentation (integer 0))
  (with-input-from-string (input string)
    (with-output-to-string (output)
      (loop :for line = (read-line input nil nil)
            :while line
            :do (format output "~a~a~%" (str:repeat indentation " ") line)))))

#|
(indent-string 4 (format nil "a~%b"))
"    a
b
"
|#

(defun leading-whitespaces (string)
  (with-input-from-string (input string)
    ;; Skip the first line
    (when (read-line input nil nil)
      (loop :for line = (read-line input nil nil)
            :while line
            :for leading-whitespaces = (position-if-not #'whitespacep line)
            :when leading-whitespaces
              :minimize leading-whitespaces))))

(defun remove-indentation (string)
  (let ((indentation (leading-whitespaces string)))
    (with-input-from-string (input string)
      (with-output-to-string (output)
        (loop :for line = (read-line input nil nil)
              :while line
              :for leading-whitespaces = (position-if-not #'whitespacep line)
              :if (and leading-whitespaces
                       (>= leading-whitespaces indentation))
                :do (write-string (subseq-displaced line indentation) output)
              :else
                :do (write-string line output)
              :do (write-char #\newline output))))))

;; TODO IIRC this function sucked, I think it might just need some
;; *print- variables set to the right thing... TO TEST
(defun print-comparison (stream string1 string2)
  "Print two (close) string in a way that the difference are easier to see."
  (let* ((mismatch (mismatch string1 string2)))
    (format stream "~&~a~%~a|~%~a"
            string1
            (if (null mismatch)
                ""
                (str:repeat mismatch "="))
            string2)))

#|
(print-comparison nil "abc" "adc")

(print-comparison nil "abce" "abcd")

(print-comparison nil
(string-downcase 'system-files)
(string-downcase 'sytsem-files))
"system-files
==|
sytsem-files"
|#


(defun summarize (string)
  "Keep only the first sentence, remove parenthesis."
  (cl-ppcre:regex-replace-all
   "\\([^)]*\\) *"
   (alexandria:if-let (position (position #\. string))
     (subseq string 0 position)
     string)
   ""))


(defun around (string position &optional (around 10))
  "Returns part of STRING, from POSITIONITION - AROUND to POSITIONITION + AROUND."
  (let* ((min-size (1+ (* 2 around)))
         (before (- position around))
         (start (max 0 before))
         (after  (+ start min-size))
         (end (min (length string) after))
         (start (max 0 (min start (- end min-size))))
         (ellipsis-left (max 0 (min 3 start)))
         (ellipsis-right (max 0 (min 3 (- (length string) end)))))
    (with-output-to-string (out)
      (loop :for i :below ellipsis-left :do (write-char #\. out))
      (write-string string out :start start :end end)
      (loop :for i :below ellipsis-right :do (write-char #\. out)))))


(alexandria:define-constant +whitespaces+
  #. (coerce '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return
               #\Rubout)
             'string)
  :test 'equal)

(defun whitespacep (char)
  "Is CHAR a whitespace?"
  (position char +whitespaces+ :test #'char=))


(defun symbol-package-qualified-name (symbol)
  "Given a SYMBOL return a string of the form package:symbol."
  (let ((*print-escape* t)
        (*package* (find-package "KEYWORD")))
    (prin1-to-string symbol)))


;;; Stream stuff

;; TODO This is horrible performance-wise, I should never use this
(defun read-stream-range (stream start end)
  "Read a subsequence from STREAM between START and END."
  (let ((current-position (file-position stream)))
    (unwind-protect
         (let ((sequence (make-string (- end start))))
           (file-position stream start)
           (read-sequence sequence stream)
           sequence)
      (file-position stream current-position))))

(defun stream-size (stream)
  "Get the total size of STREAM."
  (let ((current-position (file-position stream)))
    (when current-position
      (unwind-protect
           (progn
             (file-position stream :end) ;; TODO This might fail
             (file-position stream))
        (file-position stream current-position)))))


;;; Path stuff

(defun breeze-relative-pathname (pathname)
  "Returns a pathname relative to breeze's location."
  (if (cl-fad:pathname-relative-p pathname)
      (asdf:system-relative-pathname :breeze pathname)
      pathname))

(defun find-witness-in-parent-directories (starting-path witness
                                           &key (test #'uiop:probe-file*))
  "Search for a directory called WITNESS in current and parent
directories, recursively."
  (loop
    :repeat 1000          ; guard against infinite loop (e.g. symlink)
    :for oldpath = nil :then path
    :for path = (uiop:pathname-directory-pathname starting-path)
      :then (uiop:pathname-parent-directory-pathname path)
    :for witness-pathname = (funcall
                             test
                             (uiop:merge-pathnames* witness path))
    :until (or
            witness-pathname
            (equal oldpath path))
    :finally (return witness-pathname)))

(defun find-git-witness-folder (path)
  (find-witness-in-parent-directories path ".git/"))

(defun find-version-control-root (path)
  "Try to find the root of a directory under version control. Only
support git for now, but support for other version control systems
should be easy to add."
  (alexandria:if-let ((git-witness-directory (find-git-witness-folder path)))
    (uiop:pathname-parent-directory-pathname git-witness-directory)))

(defun find-asdf-in-parent-directories (starting-path)
  (find-witness-in-parent-directories starting-path "*.asd"
                                      :test #'directory))


;;; Sequence stuff

(defun subseq-displaced (sequence start &optional end)
  "Like subseq, but returns a displaced array instead."
  (let* ((end (or end (length sequence)))
         (size (- end start)))
    (make-array size
                :element-type (array-element-type sequence)
                :displaced-to sequence
                :displaced-index-offset start)))

(defun length>1? (list)
  "Is the length of LIST greater than 1?"
  (not (null (cdr list))))


(defun before-last (list)
  "Return the cons just before the last cons in LIST."
  (loop :for rest :on list
        :for ahead = (cddr list) :then (cdr ahead)
        :while ahead
        :finally (return
                   (when (cdr rest)
                     (car rest)))))
