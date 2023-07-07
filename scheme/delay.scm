


(define (make-promise p)
  (let ([done? #f]
        [res #f])
    (lambda ()
      (if done? res
        (begin
          (set! done? #t)
          (set! res (p)))))))

(define-macro (delay expr)
  `(make-promise (lambda () ,expr)))

(define (force delay) (delay))
