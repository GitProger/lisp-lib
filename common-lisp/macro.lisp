;(load "functional")

(defmacro defzero (name)  ; defparameter
  `(defvar ,name 0))
(defmacro defzeros (&rest names) 
  `(progn ,@(mapcar (lambda (name) `(defzero ,name)) names)))
(defmacro defvals (val &rest names) 
  `(progn ,@(mapcar (lambda (name) `(defvar ,name ,val)) names)))


(defmacro -> (val &rest forms)
  (let ((insert 
        (lambda (x form) 
          (if (listp form)
            (append (list (car form) x) (cdr form))
            (list form x)))))
        (foldl insert val forms)))

(defmacro ->> (val &rest forms)
  (let ((insert 
        (lambda (x form) 
          (if (listp form)
            (append form (list x))
            (list form x)))))
        (foldl insert val forms)))

; (-> 10 (/ 2))
; (->> 10 (/ 2))


(defvar __doc-list '())
(defun doc-find (doc-base sym)
  (if (null doc-base) 
    ""
    (let ((head (car doc-base)))
      (if (eq sym (car head)) 
        (cadr head)
        (doc-find (cdr doc-base) sym)))))

(defmacro create-doc (sym doc-string)
  `(setf __doc-list (cons (list ',sym ,doc-string) __doc-list))) ; <(cons '(',sym ,doc-string) __doc-list)> dont work:
;                                                                ;  (eq? 'aa (car '('aa))) = #f; gets '(quote aa) not 'aa
;                                                                                                           (''aa in fact)
(defmacro doc (sym) ;`(doc-find __doc-list ',sym))
  `(format t "~C~a~%" #\tab (doc-find __doc-list ',sym)))

(defmacro defn (f doc args &rest body)
  (if (stringp doc)
    `(progn
      (create-doc ,f ,(str (string f) (format nil "~a" args) ": " doc))
      (defun ,f ,args ,@body))
    `(defn ,f "-" ,doc ,@(cons args body))))


; no syntax sugar like in clojure: (defn test [a [b c]] (+ a b c)) (test 1 [2 3])
;;;;; Samples:
; (defn plus "Adds two numbers" (a b) (+ a b))
; (defn minus (a b) (- a b))


(defmacro define ((name &rest args) &rest body)
  (cons 'defun (append (cons name (list args)) body)))
  
(defmacro macro-apply-list (macro-name args) 
  `(eval (cons ',macro-name ,args)))

(defmacro macro-apply (macro-name &rest args) 
  `(eval (cons ',macro-name (list* ,@args))))
; `(eval (cons ',macro-name (apply #'list* (list ,@args)))))


