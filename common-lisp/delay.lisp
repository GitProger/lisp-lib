; (defmacro delay (form) (lambda () form)) ;(defmacro delay (form) `(lambda () ,form))
; (defun force (form) (when form (funcall form)))

(defun make-promise (p)
  (let ((donep nil)
        (res nil))
    (lambda ()
      (if donep res
        (progn 
          (setf donep t)
          (setf res (funcall p)))))))

(defmacro delay (expr)
  `(make-promise (lambda () ,expr)))

(defun force (delay) (funcall delay))
