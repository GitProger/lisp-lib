;; records are persistent
;; there is no neat way to get a list of record field names, 
;; so each setfield makes a wrap upon the old object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (defrecord point (x y))
; (defvar p1 (point 10 20))
; (defvar p2 (setfield p1 x 100)) ; no funcall syntax allowed
; (princ (funcall p1 'x)) ==> 10
; (princ (getfield p1 x)) ==> 10
; (princ (getfield p2 x)) ==> 100
; (princ (getfield p2 y)) ==> 20

(defmacro defrecord (name fields)
  `(defun ,name (&rest vals)
    (let ((mapping (mapcar #'list ',fields vals)))
      (lambda (key)
        (cadr (assoc key mapping))))))


(defmacro getfield (obj name)
  `(funcall ,obj ',name))

(defmacro setfield (obj name val)
  (lambda (key)
    (if (eq key name) val
      (eval `(getfield ,obj ,key)))))

; (defmacro setfield (obj name val)
;   `(lambda (key)
;     (if (eq key ',name) ,val
;       (getfield ,obj key))))

