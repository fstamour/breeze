(in-package #:breeze.pattern)


;;; Match substitution

(defun pattern-substitute (pattern bindings &optional (result-type 'vector))
  (cond
    ((and pattern (null bindings)) pattern)
    ((null pattern) nil)
    ((and pattern bindings)
     (check-type pattern atom)
     (check-type bindings (or binding substitutions (eql t)))
     (flet ((substitute1 (x)
              (etypecase x
                ((or simple-var var)
                 (alexandria:if-let ((binding (find-binding bindings (name x))))
                   (to binding)
                   ;; TODO this could signal a condition (binding not
                   ;; found)
                   x))
                ((or symbol number) x))))
       (if (vectorp pattern)
           (map result-type
                ;; Note: we could've recurse directly into
                ;; pattern-subtitute, but not doing so make tracing
                ;; (and debugging) tremenduously easier.
                #'(lambda (subpattern)
                    (if (vectorp subpattern)
                        (pattern-substitute subpattern bindings result-type)
                        (substitute1 subpattern)))
                pattern)
           (substitute1 pattern))))))



;;; Rules and rewrites


;; TODO "rules" would be "bidirectional" and "rewrites" wouldn't.
;; TODO (defun rule (a b) ...)
;; TODO (defun make-rewrite (antecedent consequent) ...)

#++ (progn
      (defclass abstract-rule () ())

      (defclass rule (abstract-rule) ())

      (defun make-rule (a b)
        (list :rule
              (compile-pattern a)
              (compile-pattern b)))

      (defun make-rewrite (a b)
        (list :rewrite
              (compile-pattern a)
              (compile-pattern b))))

(defun make-rewrite (pattern template)
  (cons ;; TODO use a class instead
   (compile-pattern pattern)
   (compile-pattern template)))

(defun rewrite-pattern (rewrite)
  "Get the pattern of a REWRITE rule."
  (car rewrite))

(defun rewrite-template (rewrite)
  "Get the template of a REWRITE rule."
  (cdr rewrite))
