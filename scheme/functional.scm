; (use-modules (language tree-il))
; (define (mexpand l)
;   (tree-il->scheme (macroexpand l)))

;; NO TRANSDUCERS

(define fn? procedure?)
(define int? integer?)
(define quot quotient)
(define rem remainder)
(define mod modulo)
(define (read-string s) (read (open-input-string s)))


(define (foldl f val coll)
  (if (null? coll) val
    (foldl f (f val (car coll)) (cdr coll))))

(define (foldr f val coll)
  (if (null? coll) val
    (f (foldr f (car coll) (cdr coll)) val)))

(define (reduce f coll)
  (foldl f (car coll) (cdr coll)))
(define (reducer f coll)
  (foldr f (car coll) (cdr coll)))

(define (comp1 . funs) ; all functions must be 1-arg
  (lambda (x)
    (foldl (lambda (x fn) (fn x)) x (reverse funs))))

; https://www.gnu.org/software/guile/manual/html_node/Conditionals.html
(define (comp . funs) ; all functions except for last must be 1-arg
  (case (length funs)
    ((0) identity)
    ((1) (car funs)) ; ((1) (lambda (. xs) (apply (car funs) xs)))
    ((2) (let ((f (car funs)) (g (cadr funs)))
           (lambda (. xs)
             (f (apply g xs)))))
    (else (reduce comp funs))))

(define (partial fun . args)
  (lambda (. xs)
    (apply fun (append args xs))))

;(define (identity x) x)
(define constantly const)


(define (list* a . as) 
  (cond
    [(null? as) a]
    [(and (null? (cdr as))
          (not (list? (car as))))
      (cons a (car as))]
    [else (cons a (apply list* as))]))

(define (range l r)
  (if (= l r) '()
    (cons l (range (1+ l) r))))

(define str string-append)


;(map + '(1 2 3) '(4 5 6 7 8 9))
;(mapcar #'+ '(1 2 3) '(4 5 6 7 8 9))
; CL & Clojue -> (5 7 9)
; Scheme -> error

; (define (every? predicate seq)
;   (macro-apply and (map predicate seq)))
; (define (some? predicate seq)
;   (macro-apply or (map predicate seq)))


(define (every? predicate seq)
  (if (null? seq) #t
    (and (predicate (car seq)) (every? predicate (cdr seq)))))

(define (some? predicate seq)
  (if (null? seq) #f
    (or (predicate (car seq)) (some? predicate (cdr seq)))))

(define (mapcar f . colls)
  (if (or (null? colls) (some? null? colls)) '()
    (cons (apply f (map car colls))
          (apply mapcar f (map cdr colls)))))

(define (mapcan f . colls)
  (apply append (apply map f colls)))
(define mapcat mapcan)


(define (interpose sep coll)
  (cond
    [(null? coll) '()]
    [(null? (cdr coll)) (list (car coll))]
    [else (cons (car coll) (cons sep (interpose sep (cdr coll))))]))

(define (mapcan* f . colls)
  (apply append (apply mapcar f colls)))

(define (interleave . colls) ; zip
  (apply mapcan* list colls))

(define (flatten l) ; O(n^2) !!!
  (if (null? l) '()
    (let ((h (car l)))
      (append
        (if (list? h) (flatten h) (list h)) 
        (flatten (cdr l))))))
