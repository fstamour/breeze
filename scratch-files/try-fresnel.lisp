(cl:in-package #:cl-user)

(defpackage #:breeze.try-fresnel
  (:documentation "Trying out the \"bidirectional transform\" library \"fresnel\".")
  (:use #:cl)
  (:local-nicknames (:l :fresnel/lens)))

(in-package #:breeze.try-fresnel)

(ql:quickload 'fresnel)


(fresnel/lens:

 )

(let ((lens (l:make-lens #'-))
      (x 10))
  (list
   (l:backward
    lens
    (l:forward  lens x))))
