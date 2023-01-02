;;;; 2 march 2020 - Trying to fingerprint functions by generating
;;;; randomly, but deterministically a bunch of input and hashing all
;;;; the outputs.

(in-package :breeze.user)

(ql:quickload '(random-state ironclad flexi-streams))

;;; A Bunch of simple functions
(defun x2 (x) (* x 2))
(defun double (x) (+ x x))
(defun square (x) (* x x))
(defun pow2 (x) (expt x 2))



(defvar *seed* 42)

(defun repr (x)
  "Convert anything to something that can be digested (hashed)."
  (flexi-streams:string-to-octets (format nil "~s~s" (type-of x) x)))

;; (repr 42)
;; (repr "42")

#|
Assumes that the function takes 1 32-bits argument
Could change
 * the seed
 * the random number generator
 * the number of number generated
 * the digest algorithm
 * the digest size
|#

(defun fingerprint-function (fn)
  ""
  (let ((rnd (random-state:make-generator :mersenne-twister-32 *seed*))
	(max (1- (expt 2 31)))
	(digester (ironclad:make-digest :shake256 :output-length 4)))
    (flet ((gen-int () (random-state:random-int rnd (- max) max)))
      (loop :for i :below 10000
	 :for x = (funcall fn (gen-int))
	 :do (ironclad:update-digest digester (repr x)))
      (ironclad:produce-digest digester))))



(fingerprint-function #'x2)
;; => #(207 80 46 67)

(fingerprint-function #'double)
;; => #(207 80 46 67)

(fingerprint-function #'square)
;; => #(95 88 2 241)

(fingerprint-function #'pow2)
;; => #(95 88 2 241)



;;;
;;; Variant: generate a digest for every input and keep the N smallests
;;;   very much like Min-Hash
;;;
