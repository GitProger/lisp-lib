; https://stackoverflow.com/questions/8587284/how-can-i-use-concatenation-in-scheme-without-spaces
; https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_8.html
; http://www.shido.info/lisp/scheme_syntax_e.html
; https://www.gnu.org/software/guile/manual/html_node/Macros.html 
; https://www.gnu.org/software/guile/manual/html_node/Defmacros.html
(load "functional.scm")

;(define nil (cond (#f 0)))
;(define nil '())


(defmacro defzero (name) 
  `(define ,name 0))
(defmacro defzeros (. names) 
  `(begin ,@(map (lambda (name) `(defzero ,name)) names)))
(defmacro defvals (val . names) 
  `(begin ,@(map (lambda (name) `(define ,name ,val)) names)))


(defmacro -> (val . forms)
  (let ((insert 
        (lambda (x form) 
          (if (list? form)
            (append (list (car form) x) (cdr form))
            (list form x)))))
        (foldl insert val forms)))

(defmacro ->> (val . forms)
  (let ((insert 
        (lambda (x form) 
          (if (list? form)
            (append form (list x))
            (list form x)))))
        (foldl insert val forms)))

; (-> 10 (/ 2))
; (->> 10 (/ 2))


(define __doc-list '())
(define (doc-find doc-base sym)
  (if (null? doc-base) 
    ""
    (let [(head (car doc-base))]
      (if (eq? sym (car head)) 
        (cadr head)
        (doc-find (cdr doc-base) sym)))))

(defmacro create-doc (sym doc-string)
  `(set! __doc-list (cons (list ',sym ,doc-string) __doc-list))) ; <(cons '(',sym ,doc-string) __doc-list)> dont work:
;                                                                ;  (eq? 'aa (car '('aa))) = #f; gets '(quote aa) not 'aa
;                                                                                                           (''aa in fact)
(defmacro doc (sym) ;`(doc-find __doc-list ',sym))
  `(display 
    (string-append "\t" (doc-find __doc-list ',sym) "\n")))

(define (stringify-arguments args)
  (cond
    [(null? args) ""]                                             ; no args `()`
    [(symbol? args) (string-append ". " (symbol->string args))]   ; only rest argument `(. xs)` 
    [(null? (cdr args)) (symbol->string (car args))]              ; one arg `(x)`
    [(symbol? (cdr args)) (string-append                          ; is point pair? args: `(x . y)`
                            (symbol->string (car args))
                            " . " 
                            (symbol->string (cdr args)))] 
    [else (string-append                                          ; (x ...)
            (symbol->string (car args))
            " " 
            (stringify-arguments (cdr args)))]))

; no doc -> no arg list -> need ""
(defmacro defn (f doc args . body)
  (if (string? doc)
    `(begin
      (create-doc ,f ,(string-append (symbol->string f) "(" (stringify-arguments args) "): " doc)) ; ,doc)
      (define (,f ,@args) ,@body))
    `(define (,f ,@doc) ,@(cons args body))))

; no syntax sugar like in clojure: (defn test [a [b c]] (+ a b c)) (test 1 [2 3])
;;;;; Samples:
; (defn plus "Adds two numbers" [a b] (+ a b))
; (defn minus [a b] (- a b))


(defmacro defun (name args . body)
  (cons 'define (cons (cons name args) body)))



; (letrec ((fact (lambda (n) ; letrec*
;           (if (zero? n)
;             1 
;             (* n (fact (1- n)))))))
;   (fact 6))



;; let, let* <- lambda combinations
;; cond, case ... <- if + defmacro
;; defun <- define lambda
;; map/filter/reduce/any/all <- recursion + if + car + cons + cdr
;; list* <- list <- cons

;; core:
;; defmacro
;; define
;; lambda
;; _context
;; car cdr cons
;; optional: list
