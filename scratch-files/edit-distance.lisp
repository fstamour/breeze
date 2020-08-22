;; a.k.a restricted damereau-levenshtein distance
(defun optimal-string-alignment-distance (vec-a vec-b)
  (let ((d (make-array (list (1+ (length vec-a))
                             (1+ (length vec-b)))
                       :element-type 'integer)))

    ;; Initialize the matrix
    (loop :for i :upto (length vec-a) :do
      (setf (aref d i 0) i))
    (loop :for i :upto (length vec-b) :do
      (setf (aref d 0 i) i))

    (flet ((a (index) (aref vec-a (1- index)))
           (b (index) (aref vec-b (1- index))))
      (loop :for i :from 1 :upto (length vec-a) :do
        (loop :for j :from 1 :upto (length vec-b)
              :for cost = (if (eq (a i) (b j)) 0 1) ;; aka substitution-cost
              :do
                 (setf (aref d i j) (min
                                     (1+ (aref d (1- i) j)) ;; deletion
                                     (1+ (aref d i (1- j))) ;; insertion
                                     (+ cost (aref d (1- i) (1- j))) ;; substitution
                                     ))
                 ;; transposition
                 (when (and (< 1 i) (< 1 j)
                            (eq (a i) (b (1- j)))
                            (eq (a (1- i)) (b j)))
                   (setf (aref d i j) (min (aref d i j)
                                           (+ cost (aref d (- i 2) (- j 2)))))))))
    (values (aref d (length vec-a) (length vec-b)) d)))

(optimal-string-alignment-distance
 "ca"
 "abc")
;; => 3

(optimal-string-alignment-distance
 "string a"
 "string b")
;; => 1

(optimal-string-alignment-distance
 "string"
 "string")
;; => 0

(optimal-string-alignment-distance
 "a"
 "string")
;; => 6

(optimal-string-alignment-distance
 "string"
 "a")
;; => 6

(defun optimal-string-alignment-distance2 (vec-a vec-b)
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


(optimal-string-alignment-distance2
 "ca"
 "abc")
;; => 3

(optimal-string-alignment-distance2
 "string a"
 "string b")
;; => 1

(optimal-string-alignment-distance2
 "string"
 "string")
;; => 0

(optimal-string-alignment-distance2
 "a"
 "string")
;; => 6

(optimal-string-alignment-distance2
 "string"
 "a")
;; => 6

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

(time
 (let ((input 'epxo)
       (candidate nil)
       (candidate-distance 0))
   (do-symbols (sym)
     (when (fboundp sym)
       (let ((distance (optimal-string-alignment-distance2 (symbol-name input) (symbol-name sym))))
         (when (or (not candidate)
                   (< distance candidate-distance))
           (setf candidate sym
                 candidate-distance distance)))))
   (values candidate candidate-distance)))

(time
 (let ((input 'epxo))
   (loop :for sym :being :the :symbols :of *package*
         :collect (list sym
                        (optimal-string-alignment-distance (symbol-name input) (symbol-name sym))))))
