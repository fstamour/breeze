(defpackage #:breeze.dot-checks
  (:documentation "Linter checks for period (#\.).")
  (:use #:cl #:breeze.parser))

(in-package #:breeze.dot-checks)

(breeze.checks:defcheck error-too-many-dots (node-iterator)
  :node-type breeze.parser:token
  "There can be only one dot at a time."
  (let ((start (breeze.range:start node-iterator))
        (end (breeze.range:end node-iterator)))
    ;; (when (ever))
    ;; (break "~s ~s ~s" start end (- end start))
    (when (and
           (< 1 (- end start))
           (loop
             :with source := (breeze.parser:source node-iterator)
             :for i :from start :below end
             :always (char= (char source i) #\.)))
      ;; (break "~s ~s ~s" start end (- end start))
      (breeze.diagnostics:simple-node-error
       node-iterator
       (format nil "Too many dots in ~s, there can be only one."
               (breeze.parser:node-string node-iterator))
       :replacement "."))))

#|
.
..

(breeze.parser:value
 (breeze.parser:make-node-iterator ".."))

(values
 (< 1 (- 1 0))
 (< 1 (- 2 0))
 (< 1 (- 5 0)))

(breeze.parser:value
 (breeze.parser:make-node-iterator "."))

...
|#
