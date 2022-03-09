
(uiop:define-package #:breeze.user.test
    (:documentation "Tests for breeze.user.")
  (:mix #:breeze.user #:cl #:alexandria)
  (:import-from #:breeze.test
                #:deftest
                #:is))

(in-package #:breeze.user.test)

(deftest current-packages
  (is (not (current-packages nil)))
  (is (equal (list *package*) (current-packages *package*)))
  (is (equal (current-packages 'breeze.user) (list (find-package :breeze.user))))
  (is (equal (mapcar #'find-package '(breeze cl))
             (current-packages '(breeze cl))))
  (is (equal
       (list
        (find-package :cl))
       (current-packages #'(lambda ()
                             (find-package :cl)))))
  (is
    (equal
     (sort
      (current-packages "breeze\\.(user|xref)\\.test")
      #'string<
      :key #'package-name)
     (list (find-package '#:breeze.user.test)
           (find-package '#:breeze.xref.test)))))

;; (main)
;; (next)
