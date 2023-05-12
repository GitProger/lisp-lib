(define (mapcan f . colls)
  (apply append (apply map f colls)))
(define mapcat mapcan)


(defmacro for1 (var seq . body) ; (for1 i '(1 2 3) (+ 2 (* i 10)))
  `(map 
    (lambda (,var) ,@body)
    ,seq))

(defmacro for (param . body)    ; (for (i '(1 2 3)) (+ 2 (* i 10)))
  `(map 
    (lambda (,(car param)) ,@body)
    ,(cadr param)))

(define (group2 lst)
  (if (null? lst) '()
    (cons (list (car lst) (cadr lst)) (group2 (cddr lst)))))


(defmacro for*- (bindings . body)   ; (for*- ((i '(1 2 3))) i)  ; (for* ((i '(1 2 3)) (j '(1 2 3))) (* i j))
  (if (= 1 (length bindings))
    `(for ,(car bindings) ,@body)
    `(mapcan 
      (lambda (,(caar bindings))
        (for*- ,(cdr bindings) ,@body))
      ,(cadar bindings)))) 

(defmacro for* (bindings . body)   ; (for* (i '(1 2 3) j '(1 2 3)) (* i j))
  `(for*- ,(group2 bindings) ,@body))
