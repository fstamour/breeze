
(defun levenshtein (vec-a vec-b)
  (let* ((m (length vec-a))
         (n (length vec-b))
         (diff (make-array (list 2 (1+ n)) :element-type 'integer))
         (p 0)) ;; p for pointer *shrug*

    (loop :for i :upto n :do
      (setf (aref diff 1 i) i))
    (setf (aref diff 0 0) 1)

    (flet ((a (index) (aref vec-a (1- index)))
           (b (index) (aref vec-b (1- index)))
           (diff (i j) (aref diff i j))
           (p (which) (if (zerop which)
                          p (if (= 0 p) 1 0))))
      (loop :for i :from 1 :upto m :do
        (loop :for j :from 1 :upto n
              :for cost = (if (eq (a i) (b j)) 0 1) ;; aka substitution-cost
              :do
                 (setf (aref diff (p 0) j)
                       (min
                        (1+ (diff (p 1) j))          ;; deletion
                        (1+ (diff (p 0) (1- j)))     ;; insertion
                        (+ cost (diff (p 1) (1- j))) ;; substitution
                        )))
        (when (/= m i)
          (setf p (if (zerop p) 1 0))
          (setf (aref diff (p 0) 0) (1+ i))))
      (diff (p 0) n))))

(levenshtein
 "ca"
 "abc")
;; => 3

(levenshtein
 "string a"
 "string b")
;; => 1

(levenshtein
 "string"
 "string")
;; => 0

(levenshtein
 "a"
 "string")
;; => 6

(levenshtein
 "string"
 "a")
;; => 6
