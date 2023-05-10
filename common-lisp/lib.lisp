(unless (fboundp 'identity)
  (defun identity (x) x))


(defun foldl (fun val coll)
  (if (null coll)
    val
    (foldl fun (funcall fun val (car coll)) (cdr coll))))
(defun foldr (fun val coll)
  (if (null coll)
    val
    (funcall fun val (foldr fun (car coll) (cdr coll)))))

(defun foldl0 (fun coll) ; reduce
  (foldl fun (car coll) (cdr coll)))
(defun foldr0 (fun coll) ; reducer
  (foldr fun (car coll) (cdr coll)))

(defun comp1 (&rest funs) ; all functions must be 1-arg
  (lambda (x)
    (foldl (lambda (x fn) (funcall fn x)) x (reverse funs))))

(defun comp (&rest funs) ; all functions except for last must be 1-arg
  (case (length funs)
    (0 #'identity)
    (1 (lambda (&rest xs) (apply (first funs) xs)))
    (2 (let ((f (first funs)) (g (second funs)))
         (lambda (&rest xs)
           (funcall f (apply g xs)))))
    (t (foldl0 #'comp funs)))) ; (funcall #'foldl0 #'comp funs)

(defun partial (fun &rest args)
  (lambda (&rest xs)
    (apply fun (append args xs)))) ; (nconc ; (concatenate 'list


; constantly - defined


(defun range (l r)
  (if (= l r) nil
    (cons l (range (1+ l) r))))

(defmacro macro-apply-1 (macro-name args) 
  `(eval (cons ',macro-name ,args)))

(defmacro macro-apply (macro-name &rest args) 
  `(eval (cons ',macro-name (list* ,@args))))
; `(eval (cons ',macro-name (apply #'list* (list ,@args)))))

(defun str (&rest s)
  (apply #'concatenate 'string "" s))



;; delays
(defmacro delay (form)
  `(lambda () ,form))
(defun force (form)
  (when form (funcall form)))


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

