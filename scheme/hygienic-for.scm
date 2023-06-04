(load "functional.scm")

(define-syntax for- ; (for- i '(1 2 3) (+ 2 (* i 10)))
  (syntax-rules ()
    [(_ var seq . body)
      (map (lambda (var) . body) seq)]))

(define-syntax for ; (for (i '(1 2 3)) (+ 2 (* i 10)))
  (syntax-rules ()
    [(_ (var seq) . body)
      (map (lambda (var) . body) seq)]))

(define-syntax for*- ; (for*- ((i '(1 2 3))) i)  ; (for*- ((i '(1 2 3)) (j '(1 2 3))) (* i j))
  (syntax-rules ()
    [(_ ((var range)) . body)
     (for (var range) . body)]
    [(_ ((var range) . bindings) . body)
     (mapcan 
       (lambda (var)
        (for*- bindings . body))
       range)]))

(define-syntax for* ; (for* (i '(1 2 3) j '(1 2 3)) (* i j))
  (syntax-rules ()
    [(_ (var range) . body)
     (for (var range) . body)]
    [(_ (var range . bindings) . body)
     (mapcan 
       (lambda (var)
        (for* bindings . body))
       range)]))
