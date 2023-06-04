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

(define-macro (defn f doc args . body)
  (if (string? doc)
    `(begin
      (create-doc ,f ,(string-append (symbol->string f) " " (format #f "~a" args) ": " doc)) ; ,doc)
      (define (,f ,@args) ,@body))
    `(defn ,f "-" ,doc ,@(cons args body))))

(define-syntax defun
  (syntax-rules ()
    [(_ name args . body)
      (define (name . args) . body)]))

(define-syntax macro-apply-list
  (syntax-rules ()
    [(_ macro-name args)
     (eval (cons 'macro-name args) (interaction-environment))]))


(define-macro (macro-apply macro-name . args) 
  `(eval (cons ',macro-name (list* ,@args)) (interaction-environment)))
