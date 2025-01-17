
(in-package #:breeze.listener)


;; TODO Use a heap to get the N smallest values!
;; TODO Put that into utils?
(defmacro minimizing ((var
                       &key
                         (score-var (gensym "score"))
                         tracep)
                      &body body)
  "Creates both a variable (let) and a function (flet) to keep track
of the instance of that had the smallest score."
  (check-type var symbol)
  `(let  ((,var nil)
          (,score-var))
     (flet ((,var (new-candidate new-score)
              ,@(when tracep
                  `((format *debug-io* "~&new-candidate: ~s new-score: ~s"
                            new-candidate new-score)))
              (when (and new-score
                         (or
                          ;; if it wasn't initialized already
                          (null ,var)
                          ;; it is initialized, but score is better
                          (< new-score ,score-var)))
                (setf ,var new-candidate
                      ,score-var new-score))))
       ,@body
       (values ,var ,score-var))))


(defun find-most-similar-symbol (input)
  (minimizing (candidate)
    ;; TODO do-symbols only iterate on *package*
    (do-symbols (sym)
      (when (fboundp sym)
        (candidate sym
                   (breeze.string:optimal-string-alignment-distance*
                    input
                    (string-downcase sym)
                    3))))))

;; (find-most-similar-symbol "prin") ;; => princ, 1

(defun find-most-similar-package (input)
  (minimizing (candidate)
    (loop :for package in (list-all-packages)
          :for package-name = (package-name package) :do
            (loop :for name in `(,package-name ,@(package-nicknames package)) :do
              (candidate name
                         (breeze.string:optimal-string-alignment-distance*
                          input
                          (string-downcase name)
                          3))))))

#+ (or)
(progn
  (find-most-similar-package "breeze.util")
  ;; => breeze.utils, 1

  (find-most-similar-package "commmon-lisp")
  ;; => "COMMON-LISP", 1
  )

(defun find-most-similar-class (input)
  (minimizing (candidate)
    (do-symbols (sym)
      (when (classp sym)
        (candidate sym
                   (breeze.string:optimal-string-alignment-distance*
                    input
                    (string-downcase sym)
                    3))))))

(defvar *last-invoked-restart* nil
  "For debugging purposes only")

(defun resignal-with-suggestion-restart (input candidate condition)
  ;; Ok, this is messy as hell, but it works
  (unless
      ;; We install a new restart
      (with-simple-restart (use-suggestion
                            "Use \"~a\" instead of \"~a\"."
                            candidate input)
        ;; with-simple-restart returns the _last evaluated_ form
        t
        ;; Then we signal the condition again
        (error condition))
    ;; with-simple-restart will return nil and t if the restart was
    ;; invoked
    (let ((use-value (find-restart 'use-value condition)))
      (setf *last-invoked-restart* (list candidate))
      (format *debug-io* "~&About to invoke the restart ~s with the value ~s."
              use-value
              candidate)
      (invoke-restart use-value candidate))))

(defun suggest (input candidate condition)
  (message "Did you mean \"~a\"?" candidate)
  (when candidate
    (let ((restart (find-restart 'use-value condition)))
      (or
       (and restart (resignal-with-suggestion-restart
                     input candidate condition))
       (warn "Did you mean \"~a\"?~%~a"
             candidate
             (breeze.string:indent-string
              2
              (breeze.string:print-comparison
               nil
               (string-downcase candidate)
               input)))))))

(defgeneric condition-suggestion-input (condition)
  (:documentation "Get input for \"find-most-similar-*\" functions from a condition")
  ;; Default implementation
  (:method (condition)
    (cell-error-name condition))
  (:method ((condition undefined-function))
    (format *debug-io* "~&1")
    (cell-error-name condition))
  (:method ((condition package-error))
    (let ((package-designator
            (package-error-package condition)))
      (if (stringp package-designator)
          package-designator
          #+sbcl ;; only tested on sbcl
          (car
           (slot-value condition
                       'sb-kernel::format-arguments)))))
  #+sbcl
  (:method ((condition sb-ext:package-does-not-exist))
    (package-error-package condition))
  #+sbcl
  (:method ((condition sb-pcl:class-not-found-error))
    (sb-kernel::cell-error-name condition)))

;; (trace condition-suggestion-input)

(defmacro defun-suggest (types)
  `(progn
     ,@(loop
         :for type :in types
         :collect
         `(defun ,(symbolicate 'suggest- type) (condition)
            (let* ((input (string-downcase (condition-suggestion-input condition)))
                   (candidate (,(symbolicate 'find-most-similar- type) input)))
              #+ (or)
              (format *debug-io*
                      ,(format nil
                               "~~&candidate ~(~a~): ~~s"
                               type)
                      candidate)
              (if candidate
                  (suggest input candidate condition)
                  (error condition)))))))

(defun-suggest
    (symbol
     package
     class))

(defvar *last-condition* nil
  "For debugging purposose only.")


(defun call-with-correction-suggestion (function)
  "Funcall FUNCTION wrapped in a handler-bind form that suggest corrections."
  (handler-bind
      ((error #'(lambda (condition)
                  (setf *last-condition* condition)
                  (error condition))))
    (handler-bind
        ;; The order is important!!!
        ((undefined-function #'suggest-symbol)
         #+sbcl (sb-ext:package-does-not-exist #'suggest-package)
         #+sbcl (sb-int:simple-reader-package-error #'suggest-symbol)
         #+ (or)
         (package-error #'suggest-package)
         #+sbcl
         (sb-pcl:class-not-found-error #'suggest-class))
      (funcall function))))
