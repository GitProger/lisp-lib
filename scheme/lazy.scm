
(define (uniforce x)
  ((if (promise? x) force identity) x))

(define (lazy-car l)
  (uniforce (car l)))

(define (lazy-cdr l)
	(uniforce (cdr l)))

(define (lazy-cons h t)
	(cons h (delay t)))

(define (lazy-seq l)
  (if (null? l)
    l
   	(lazy-cons (car l) (lazy-seq (cdr l)))))

(define (lazy-seq->list lazy)
  (if (null? lazy)
    '()
    (cons (lazy-car lazy)
          (lazy-seq->list (lazy-cdr lazy)))))

(define (lazy-nth s i) 
  (if (zero? i)
    (lazy-car s)
    (lazy-nth (lazy-cdr s) (1- i))))

(define (lazy-seq-gen start next)
  (if (null? start) '()
    (cons start (delay (lazy-seq-gen (next start) next)))))

(define (lazy-take n seq) ; returns lazy-seq
  (if (or (zero? n) (null? seq)) '() 
    (lazy-cons (lazy-car seq) (lazy-take (1- n) (lazy-cdr seq)))))

(define (lazy-take-while predicate seq)
  (let ((x (lazy-car seq)))
    (if (predicate x)
       (lazy-cons x (lazy-take-while predicate (lazy-cdr seq)))
      '())))

(define (lazy-cycle c)
  (let ((cur (lazy-cdr c)))
    (lazy-seq-gen (lazy-car c)
      (lambda (_)
        (when (null? cur) 
          (set! cur c))
        (let ((res (lazy-car cur)))
          (set! cur (lazy-cdr cur))
          res)))))

; (lazy-seq->list (lazy-take 5 (lazy-cycle '(1 2))))
; (lazy-seq->list (lazy-take 5 (lazy-cycle (lazy-seq '(1 2)))))
; (lazy-seq->list (lazy-take 5 (lazy-cycle (range 1 5 1))))
; (lazy-seq->list (lazy-take 17 (lazy-cycle (lazy-range 1 6 1))))

(define (correct-range? l r s)
  (or (and (< l r) (positive? s))
      (and (> l r) (negative? s))
      (zero? s)))

(define (lazy-range start end step)
  (if (correct-range? start end step)
    (lazy-seq-gen start
      (lambda (it)
        (if (correct-range? (+ it step) end step)
          (+ it step)
          '())))
    '()))

(define (xrange l r . s)
  (let ((step (if (null? s) 1 (car s))))
    (if (correct-range? l r step)
      (cons l (xrange (+ l step) r step))
      '())))

