;;;; String manipulation utilities

(defpackage #:breeze.string
  (:documentation "String manipulation utilities")
  (:use #:cl)
  (:import-from #:breeze.utils
                #:subseq-displaced)
  (:export
   #:string-designator
   #:around
   #:optimal-string-alignment-distance
   #:optimal-string-alignment-distance*
   #:repeat-string
   #:split-by-newline
   #:indent-string
   #:remove-indentation
   #:print-comparison
   #:summarize
   #:+whitespaces+
   #:trim-whitespace
   #:whitespacep
   #:symbol-package-qualified-name
   #:ensure-prefix
   #:ensure-suffix
   #:ensure-circumfix))

(in-package #:breeze.string)

(deftype string-designator () '(or string character symbol))

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



(defun repeat-string (n string &optional stream)
  (if stream
      (loop :repeat n :do (write-string string stream))
      (with-output-to-string (output)
        (repeat-string n string output))))



(defun split-by-newline (string)
  (uiop:split-string string :separator '(#\Newline)))

#++
(split-by-newline "a
b
c")



(defun indent-string (indentation string)
  "Prepend INDENTATION spaces at the beginning of each line in STRING."
  (check-type indentation (integer 0))
  (with-input-from-string (input string)
    (with-output-to-string (output)
      (loop :for line = (read-line input nil nil)
            :while line
            :do
               (repeat-string indentation " " output)
               (write-string line output)
               (terpri output)))))

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
                (repeat-string mismatch "="))
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


;; all this to get rid of cl-ppcre xD
(defun remove-parentheses (string)
  "Return new string with the parts between parentheses removed, along
with the spaces following the closing parentheses. Do not support
nested parentheses."
  (with-output-to-string (o)
    (loop
      :with skipping
      :for c across string
      :do (cond
            ;; Opening paren
            ((and (not skipping)
                  (char= #\( c))
             (setf skipping c))
            ;; Closing paren
            ((and skipping
                  (char= #\) c))
             (setf skipping c))
            ;; Common case
            ((not skipping) (write-char c o))
            ;; Non-space char after closing paren
            ((and skipping
                  (char= #\) skipping)
                  (char/= #\Space c))
             (setf skipping nil)
             (write-char c o))))))

(defun summarize (string)
  "Keep only the first sentence, remove parenthesis."
  (remove-parentheses
   (alexandria:if-let (position (position #\. string))
     (subseq string 0 position)
     string)))


;; This is a good candidate for a funtion where the unit tests would
;; provide great examples for the documentation.
(defun around (string position &optional (around 10))
  "Returns part of STRING, from POSITIONITION - AROUND to POSITIONITION +
AROUND. Add elipseses before and after if necessary."
  (let* ((min-size (1+ (* 2 around)))
         (before (- position around))
         (start (max 0 before))
         (after (+ start min-size))
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

(defun trim-whitespace (string)
  (string-trim +whitespaces+ string))

(defun symbol-package-qualified-name (symbol)
  "Given a SYMBOL return a string of the form package:symbol."
  (let ((*print-escape* t)
        (*package* (find-package "KEYWORD")))
    (prin1-to-string symbol)))


(defun ensure-prefix (prefix string)
  (if (alexandria:starts-with-subseq prefix string)
      string
      (concatenate 'string prefix string)))

#++
(and (equal
      "*a"
      (ensure-prefix "*" "a"))
     (equal (ensure-prefix "*" "a")
            (ensure-prefix "*" "*a")))

(defun ensure-suffix (suffix string)
  (if (alexandria:ends-with-subseq suffix string)
      string
      (concatenate 'string string suffix)))

#++
(and (equal
      "a*"
      (ensure-suffix "*" "a"))
     (equal (ensure-suffix "*" "a")
            (ensure-suffix "*" "a*")))

(defun ensure-circumfix (circumfix string)
  (ensure-suffix circumfix (ensure-prefix circumfix string)))

#++
(equal "*a*" (ensure-circumfix "*" "a"))
