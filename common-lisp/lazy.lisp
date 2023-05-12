
(defmacro delay (form)
  `(lambda () ,form))

(defun force (form)
  (when form (funcall form)))

(defun uniforce (form)
  (if (functionp form)
    (funcall form)
    form))


(defun lazy-car (l)
  (uniforce (car l)))

(defun lazy-cdr (l)
  (uniforce (cdr l)))

(defun lazy-cons (head tail)
  (cons head (delay tail)))

(defun lazy-seq (l)
  (if (null l) 
    l
    (lazy-cons (car l) (lazy-seq (cdr l)))))

(defun lazy-seq->list (lazy)
  (if (null lazy)
    '()
    (cons (lazy-car lazy)
          (lazy-seq->list (lazy-cdr lazy)))))

(defun lazy-nth (s i) 
  (if (zerop i)
    (lazy-car s)
    (lazy-nth (lazy-cdr s) (1- i))))

(defun lazy-seq-gen (start next)
  (if (null start) '()
    (cons start (delay (lazy-seq-gen (funcall next start) next)))))

(defun lazy-take (n seq) ; returns lazy-seq
  (if (or (zerop n) (null seq)) '() 
    (lazy-cons (lazy-car seq) (lazy-take (1- n) (lazy-cdr seq)))))

(defun lazy-take-while (predicate seq)
  (let ((x (lazy-car seq)))
    (if (funcall predicate x)
       (lazy-cons x (lazy-take-while predicate (lazy-cdr seq)))
      '())))

(defun lazy-cycle (c)
  (let ((cur (lazy-cdr c)))
    (lazy-seq-gen (lazy-car c)
      #'(lambda (_)
        (declare (ignore _))
        (when (null cur) 
          (setf cur c))
        (let ((res (lazy-car cur)))
          (setf cur (lazy-cdr cur))
          res)))))

; (lazy-seq->list (lazy-take 5 (lazy-cycle '(1 2))))
; (lazy-seq->list (lazy-take 5 (lazy-cycle (lazy-seq '(1 2)))))
; (lazy-seq->list (lazy-take 5 (lazy-cycle (xrange 1 5 1))))
; (lazy-seq->list (lazy-take 17 (lazy-cycle (lazy-range 1 6 1))))

(defun correct-rangep (l r s)
  (or (and (< l r) (plusp s))
      (and (> l r) (minusp s))
      (zerop s)))

(defun lazy-range (start end step)
  (if (correct-rangep start end step)
    (lazy-seq-gen start
      (lambda (it)
        (if (correct-rangep (+ it step) end step)
          (+ it step)
          '())))
    '()))

(defun xrange (l r &optional (s 1))
  (if (correct-rangep l r s)
    (cons l (xrange (+ l s) r s))
    '()))

