(use-modules (ice-9 textual-ports))

(define (spit name str)
  (let ((handle (open-output-file name)))
    (display str handle)
    (close-output-port handle)))

(define (slurp name)
  (let* ((handle (open-input-file name))
         (content (get-string-all handle)))
    (close-input-port handle)
    content))

