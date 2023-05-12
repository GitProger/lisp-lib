; mapcan - defined


;; fors
(defmacro for1 (var seq &rest body) ; (for1 i '(1 2 3) (+ 2 (* i 10)))
  `(mapcar 
    (lambda (,var) ,@body)
    ,seq))

(defmacro for ((var seq) &rest body) ; (for (i '(1 2 3)) (+ 2 (* i 10)))
  `(mapcar 
    (lambda (,var) ,@body)
    ,seq))

; (defmacro for (param &rest body)    ; (for (i '(1 2 3)) (+ 2 (* i 10)))
;   `(mapcar
;     (lambda (,(car param)) ,@body)
;     ,(cadr param)))

(defun group2 (lst)
  (if (null lst) nil
    (cons (list (car lst) (cadr lst)) (group2 (cddr lst)))))

(defmacro for*- (bindings &rest body)   ; (for*- ((i '(1 2 3))) i)  ; (for* ((i '(1 2 3)) (j '(1 2 3))) (* i j))
  (if (= 1 (length bindings))
    `(for ,(car bindings) ,@body)
    `(mapcan 
      (lambda (,(caar bindings))
        (for*- ,(cdr bindings) ,@body))
      ,(cadar bindings)))) 

(defmacro for* (bindings &rest body)   ; (for* (i '(1 2 3) j '(1 2 3)) (* i j))
  `(for*- ,(group2 bindings) ,@body))

