(define (range l r)
  (if (= l r) '()
    (cons l (range (1+ l) r))))

; defun ([-\w]*)\s*\(([^\)]*)\)   --> `define (\1 \2)
;; lists:

(define (transpose M)
  (apply map list M))

(define (dot a b)
  (apply + (map * a b)))

(define (mulv M v)
  (map (lambda (x) (dot x v)) M))

(define (mul A B)
  (transpose 
    (map
      (lambda (v) (mulv A v))
      (transpose B))))

(define (range l r)
  (if (= l r) '()
    (cons l (range (1+ l) r))))

(define (I n)
  (if (list? n)
    (I (length n))
    (let ((r (range 0 n)))
      (map
        (lambda (i) 
          (map 
            (lambda (j) 
              (if (= i j) 1 0))
            r))
        r))))

(define (pow M p)
  (if (zero? p) 
    (I M)
    (let ((h (pow M (quotient p 2))))
      (mul (mul h h) (if (odd? p) M (I M))))))
      
;; vectors:

(define (mapv f . colls)
  (list->vector
    (apply map f 
      (map (lambda (v) 
             (if (vector? v) (vector->list v) v))
           colls))))


(define (transpose-v M)
  (apply mapv vector (vector->list M)))

(define (dot-v a b)
  (apply + (vector->list (mapv * a b))))

(define (mulv-v M v)
  (mapv (lambda (x) (dot-v x v)) M))

(define (mul-v A B)
  (transpose-v
    (mapv
      (lambda (v) (mulv-v A v)) 
      (transpose-v B))))

(define (I-v n)
  (if (vector? n)
    (I-v (vector-length n))
    (let ((r (range 0 n)))
      (mapv
        (lambda (i) 
          (mapv
            (lambda (j) 
              (if (= i j) 1 0))
            r))
        r))))

(define (pow-v M p)
  (if (zero? p) 
    (I-v M)
    (let ((h (pow-v M (quotient p 2))))
      (mul-v (mul-v h h) (if (odd? p) M (I-v M))))))
