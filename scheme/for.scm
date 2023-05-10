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

; scheme@(guile-user)> (for*- ((i r)) i)
; $1 = (1 2 3)
; scheme@(guile-user)> (for*- ((i r) (j r)) i)
; $2 = (1 1 1 2 2 2 3 3 3)
; scheme@(guile-user)> (for*- ((i r) (j r)) (- i j))
; $3 = (0 -1 -2 1 0 -1 2 1 0)
; scheme@(guile-user)> (for* (i r j r) (- i j))
; $4 = (0 1 2 -1 0 1 -2 -1 0)



; (define r '(0 1 2 3))
; (mapcan
;   (lambda (j)
;     (map 
;       (lambda (i)
;         (* i j))
;       r))
;   r)

; (def r '(0 1 2 3))
; (mapcat
;   (fn [j]
;     (map 
;       (fn [i]
;         (* i j))
;       r))
;   r)



