;; records are persistent
;; there is no neat way to get a list of record field names, 
;; so each setfield makes a wrap upon the old object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; how it works:
;; example:
; (define (counter s) 
;   (let ((x 0))
;     (lambda ()
;       (set! x (1+ x))
;       x)))
;
; (define (point x y) 
;   (lambda (sym)
;     (eval sym (interaction-environment))))
;
; (defrecord point (x y))
; ((point 1 2) 'x) == 1

(define-macro (defrecord name fields)
  `(define (,name . vals)
    (let ((mapping (map list ',fields vals)))
      (lambda (key)
        (cadr (assoc key mapping))))))

(define-macro (getfield obj name)
  `(,obj ',name))

; (define (point . vals)
;   (let ((mapping (map list '(x y z) vals)))
;     (lambda (key)
;       (cadr (assoc key mapping)))))
