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

; lambda (. x) <==> lambda x
(defmacro defn (f doc args . body)
  (if (string? doc)
    `(begin
      (create-doc ,f ,(string-append (symbol->string f) " " (format #f "~a" args) ": " doc)) ; ,doc)
      (define (,f ,@args) ,@body))
    `(defn ,f "-" ,doc ,@(cons args body))))


; no syntax sugar like in clojure: (defn test [a [b c]] (+ a b c)) (test 1 [2 3])
;;;;; Samples:
; (defn plus "Adds two numbers" [a b] (+ a b))
; (defn minus [a b] (- a b))


(defmacro defun (name args . body)
  (cons 'define (cons (cons name args) body)))
