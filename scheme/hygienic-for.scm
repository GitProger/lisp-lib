(load "functional.scm")

(define-syntax for1 ; (for1 i '(1 2 3) (+ 2 (* i 10)))
  (syntax-rules ()
    [(_ var seq . body)
      (map (lambda (var) . body) seq)]))

(define-syntax for ; (for (i '(1 2 3)) (+ 2 (* i 10)))
  (syntax-rules ()
    [(_ (var seq) . body)
      (map (lambda (var) . body) seq)]))

(define (group2 lst)
  (if (null? lst) '()
    (cons (list (car lst) (cadr lst)) (group2 (cddr lst)))))


(define-macro (for*- bindings . body)   ; (for*- ((i '(1 2 3))) i)  ; (for*- ((i '(1 2 3)) (j '(1 2 3))) (* i j))
  (if (= 1 (length bindings))
    `(for ,(car bindings) ,@body)
    `(mapcan 
      (lambda (,(caar bindings))
        (for*- ,(cdr bindings) ,@body))
      ,(cadar bindings)))) 

(define-macro (for* bindings . body)   ; (for* (i '(1 2 3) j '(1 2 3)) (* i j))
  `(for*- ,(group2 bindings) ,@body))
