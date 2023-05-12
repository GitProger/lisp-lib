(defun range (l r)
  (if (= l r) nil
    (cons l (range (1+ l) r))))

;; lists:

(defun transpose (M)
  (apply #'mapcar #'list M))

(defun dot (a b)
  (apply #'+ (mapcar #'* a b)))

(defun mulv (M v)
  (mapcar (lambda (x) (dot x v)) M))

(defun mul (A B)
  (transpose 
    (mapcar
      (lambda (v) (mulv A v)) 
      (transpose B))))

(defun I (n)
  (if (listp n)
    (I (length n))
    (let ((r (range 0 n)))
      (mapcar 
        (lambda (i) 
          (mapcar 
            (lambda (j) 
              (if (= i j) 1 0))
            r))
        r))))

(defun pow (M p)
  (if (zerop p) 
    (I M)
    (let ((h (pow M (floor p 2))))
      (mul (mul h h) (if (oddp p) M (I M))))))
      
;; vectors:

(defun transpose-v (M)
  (apply #'map 'vector #'vector (coerce M 'list)))

(defun dot-v (a b)
  (apply #'+ (coerce (map 'vector #'* a b) 'list)))

(defun mulv-v (M v)
  (map 'vector (lambda (x) (dot-v x v)) M))

(defun mul-v (A B)
  (transpose-v 
    (map 'vector
      (lambda (v) (mulv-v A v)) 
      (transpose-v B))))

(defun I-v (n)
  (if (vectorp n)
    (I-v (length n))
    (let ((r (range 0 n)))
      (map 'vector
        (lambda (i) 
          (map 'vector
            (lambda (j) 
              (if (= i j) 1 0))
            r))
        r))))

(defun pow-v (M p)
  (if (zerop p) 
    (I-v M)
    (let ((h (pow-v M (floor p 2))))
      (mul-v (mul-v h h) (if (oddp p) M (I-v M))))))
      

