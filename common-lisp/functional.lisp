;; NO TRANSDUCERS

(unless (fboundp 'identity)
  (defun identity (x) x))

(defun fnp (x) (functionp x))
(defun intp (x) (integerp x))
(defun quot (x y) (floor x y))
; rem - defined
; mod - defined
; read-string read-from-string

(defun foldl (fun val coll)
  (if (null coll)
    val
    (foldl fun (funcall fun val (car coll)) (cdr coll))))
(defun foldr (fun val coll)
  (if (null coll)
    val
    (funcall fun (foldr fun (car coll) (cdr coll)) val)))

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
; list* - defined
; mapcan - defined

(defun range (l r)
  (if (= l r) nil
    (cons l (range (1+ l) r))))

(defun str (&rest s)
  (apply #'concatenate 'string "" s))


; every? - every
; some? - some
; mapcar - defined
; mapcan - defined


(defun interpose (sep coll)
  (cond
    ((null coll) '())
    ((null (cdr coll)) (list (car coll)))
    (:else (cons (car coll) (cons sep (interpose sep (cdr coll)))))))

(defun interleave (&rest colls) ; zip
  (if (null colls) nil
    (apply #'mapcan #'list colls)))

(defun flatten (l) ; O(n^2) !!!
  (if (null l) nil
    (let ((h (car l)))
      (append
        (if (listp h) (flatten h) (list h)) 
        (flatten (cdr l))))))
