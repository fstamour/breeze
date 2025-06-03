
(defpackage #:breeze.utils
  (:use :cl)
  (:documentation "Utilities")
  (:import-from #:alexandria #:symbolicate)
  (:export
   #:walk
   #:walk-car
   #:walk-list
   #:nrun
   #:package-apropos)
  (:export
   #:before-last
   #:subseq-displaced
   #:length>1?
   #:with-collectors)
  (:export
   #:stream-size)
  (:export
   #:breeze-relative-pathname
   #:find-version-control-root
   #:find-asdf-in-parent-directories))

(in-package #:breeze.utils)


;;; Other

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


;; TODO make tests
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




;; TODO add tests...
(defun nrun (list predicate)
  "Destructively extract the first run of elements that satisfies
PREDICATE, if the first element of LIST satisfies PREDICATE, . Returns
the run and update LIST's first cons to point to the last element of
the run."
  (when (and list
             ;; I didn't want to think about the case where there is
             ;; no first run.
             (funcall predicate (first list)))
    (loop
      ;; keep track of the last "head of list", so we can 1: return
      ;; the last element for which PREDICATE was true and 2: set its
      ;; cdr to nil to make the list end there.
      :for last-cons = nil :then cons
      ;; iterate cons by cons
      :for cons :on list
      ;; extract the next element to check
      :for el = (first cons)
      ;; does the new element pass the test?
      :while (funcall predicate el)
      ;; once we find an element that doesn't pass the test, we
      ;; <strong>update</strong> LIST so that the first element is now
      ;; the <em>last</em> element that did pass the test. That way
      ;; the caller can easily access the first and last element of
      ;; the run; and it happens that it plays very well with (loop
      ;; :for :on), because that construct would skip the next
      ;; element...
      :finally
         (let (;; Copy the original first cons of the list
               (run (cons (car list) (cdr list))))
           (setf
            ;; sever the link between the run and the rest of the list
            (cdr last-cons) nil
            ;; update the first element of the list
            (car list) (car last-cons)
            ;; update the rest of the list
            (cdr list) cons)
           (return run)))))

#++ ;; Demonstrate that nrun modifies the list
(let ((list (copy-seq '(1 3 4 5))))
  (values (nrun list #'oddp)
          list))
;; => (1 3), (3 4 5)



(defun package-apropos (search-string)
  "Compute a list of package that contains the search-string."
  (remove-if-not #'(lambda (package)
                     (search search-string (package-name package)
                             :test #'string-equal))
                 (list-all-packages)))


;;; Path stuff

(defun breeze-relative-pathname (pathname)
  "Returns a pathname relative to breeze's location."
  (if (uiop:relative-pathname-p pathname)
      (asdf:system-relative-pathname :breeze pathname)
      pathname))

;; TODO move the rest of the page into workflow.lisp (I think)

;; TODO This is kinda like "locate-dominating-file" in emacs, it might
;; be a better name?
;;
;; TODO FIXME I got the condition "Invalid use of :BACK after
;; :ABSOLUTE." when I called this on breeze's directory
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

;; TODO make tests
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
