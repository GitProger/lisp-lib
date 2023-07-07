(load "functional.scm")

(define-syntax defzero
  (syntax-rules ()
    [(_ name) (define name 0)]))
(define-syntax defzeros
  (syntax-rules ()
    [(_ name ...) (begin (define name 0) ...)]))
(define-syntax defvals
  (syntax-rules ()
    [(_ val name ...) (begin (define name val) ...)]))


(define-syntax _i->
  (syntax-rules ()
    [(_ x (form ...)) (form x ...)] ; [(_ x (form . args)) (form x . args)]
    [(_ x form) (form x)]))
(define-syntax ->
  (syntax-rules ()
    [(_ x) x]
    [(_ x form . forms)
     (-> (_i-> x form) . forms)]))

(define-syntax _i->>
  (syntax-rules ()
    [(_ x (form ...)) (form ... x)] ; [(_ x (form . args)) (form . args x)]
    [(_ x form) (form x)]))
(define-syntax ->>
  (syntax-rules ()
    [(_ x) x]
    [(_ x form . forms)
     (->> (_i->> x form) . forms)]))



(define __doc-list '())
(define (doc-find doc-base sym)
  (if (null? doc-base) 
    ""
    (let [(head (car doc-base))]
      (if (eq? sym (car head)) 
        (cadr head)
        (doc-find (cdr doc-base) sym)))))

(define-syntax create-doc 
  (syntax-rules ()
    [(_ sym doc-string)
      (set! __doc-list (cons (list 'sym doc-string) __doc-list))]))

(define-syntax doc
  (syntax-rules ()
    [(_ sym)
      (display (string-append "\t" (doc-find __doc-list 'sym) "\n"))]))

; (define-macro (defn f doc args . body)
;   (if (string? doc)
;     `(begin
;       (create-doc ,f ,(string-append (symbol->string f) " " (format #f "~a" args) ": " doc)) ; ,doc)
;       (define (,f ,@args) ,@body))
;     `(defn ,f "-" ,doc ,@(cons args body))))


(define-syntax defn-doc
  (syntax-rules ()
    [(_ f doc args . body)
     (begin
      (create-doc f (string-append (symbol->string 'f) " " (format #f "~a" 'args) ": " doc))
      (define (f . args) . body))]))

; (define-syntax defn
;   (syntax-rules ()
;     [(_ f doc args . body)
;      (if (string? doc)
;       (defn-doc f doc args . body)
;       (defn-doc f "-" doc . (cons 'args 'body)))]))

(define-syntax defn
  (syntax-rules ()
    [(_ f doc args . body)
     (let* ((ok (string? doc))
            (true-doc (if ok doc "-"))
            (true-args (if ok 'args doc))
            (true-body (if ok 'body (cons 'args 'body))))
      (eval `(defn-doc f ,true-doc ,true-args . ,true-body) (interaction-environment)))]))


(define-syntax defun
  (syntax-rules ()
    [(_ name args . body)
      (define (name . args) . body)]))


(define-syntax macro-apply-list
  (syntax-rules ()
    [(_ macro-name args)
     (eval (cons 'macro-name args) (interaction-environment))]))


; (define-macro (macro-apply macro-name . args) 
;   `(eval (cons ',macro-name (list* ,@args)) (interaction-environment)))

; (define (leval e) (eval e (interaction-environment)))
; (define-syntax macro-apply
;   (syntax-rules ()
;     [(_ macro-name . args)
;       (leval (cons 'macro-name (leval (cons 'list* 'args))))]))

(define-syntax macro-apply
  (syntax-rules ()
    [(_ macro-name . args)
      (macro-apply-list macro-name (eval (cons 'list* 'args) (interaction-environment)))]))


; (mexpand '(macro-apply and 1 2 '(3 4)))
; (macro-apply and 1 2 '(3 4))
