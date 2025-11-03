(in-package #:breeze.pattern)

;; TODO there's no syntax for "repetition", only for "maybe" and "zero-or-more"


;;; Pattern compilation from lists and symbols to vectors and structs

(defun symbol-starts-with (symbol prefix)
  (and (symbolp symbol)
       (etypecase prefix
         (character (char= prefix (char (symbol-name symbol) 0)))
         (string (alexandria:starts-with-subseq prefix
                                                (symbol-name symbol))))))

(defun var-symbol-p (x)
  "Does the symbol X represents a \"var\" pattern?"
  (symbol-starts-with x #\?))

(defun multi-valued-var-symbol-p (x)
  "Does the symbol X represents a multi-valued \"var\" pattern?"
  (symbol-starts-with x "?*"))

(defun wildcard-symbol-p (x)
  "Does the symbol X represents an \"wildcard\" pattern?"
  (symbol-starts-with x #\_))

(declaim (inline wildp))
(defun wildp (x)
  (eq x :wild))

(defun compile-pattern (pattern)
  "Compiles a PATTERN (specified as a list)"
  (%compile-pattern pattern))

;; Default: leave as-is
(defmethod %compile-pattern (pattern) pattern)

;; Compile symbols
(defmethod %compile-pattern ((pattern symbol))
  (cond
    ((var-symbol-p pattern) (var pattern))
    ((wildcard-symbol-p pattern) (wildcard))
    (t pattern)))

;; Compile lists
(defmethod %compile-pattern ((pattern cons))
  ;; Dispatch to another method that is eql-specialized on the first
  ;; element of the list.
  (compile-compound-pattern (first pattern) pattern))

;; Default list compilation: recurse and convert to vector.
(defmethod compile-compound-pattern (token pattern)
  (map 'vector #'%compile-pattern pattern))

(defmethod compile-compound-pattern ((token (eql :maybe)) pattern)
  "Compile (:maybe ...)"
  (destructuring-bind (sub-pattern)
      (rest pattern)
    (maybe sub-pattern)))

(defmethod compile-compound-pattern ((token (eql :var)) pattern)
  "Compile (:var name [pattern])"
  (destructuring-bind (name sub-pattern &key multi-valued-p)
      (rest pattern)
    ;; TODO multi-values
    (var name (compile-pattern sub-pattern))))

(defmethod compile-compound-pattern ((token (eql :zero-or-more)) pattern)
  "Compile (:zero-or-more ...)"
  (zero-or-more (%compile-pattern (rest pattern))))

(defmethod compile-compound-pattern ((token (eql :either)) patterns)
  "Compile (:either ...)"
  (either (%compile-pattern (rest patterns))))

(defmethod compile-compound-pattern ((token (eql :symbol)) pattern)
  "Compile (:symbol [name [package-name [qualification]]])"
  (destructuring-bind (&optional
                         (symbol-name-pattern :wild)
                         (package-name-pattern :wild)
                         (qualification-pattern :wild))
      (rest pattern)
    (sym package-name-pattern
         symbol-name-pattern
         qualification-pattern)))
