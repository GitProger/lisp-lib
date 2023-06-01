(define-syntax defrecord
  (syntax-rules ()
    ((_ name fields)
      (define (name . vals)
        (let ((mapping (map list 'fields vals)))
          (lambda (key)
            (cadr (assoc key mapping))))))))

(define-syntax get-field
  (syntax-rules ()
    ((_ obj name)
      (obj 'name))))

(define-syntax set-field
  (syntax-rules ()
    ((_ obj name val)
      (lambda (key)
        (if (eq? key 'name) val
          (obj key))))))
