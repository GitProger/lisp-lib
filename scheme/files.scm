;; https://ru.wikipedia.org/wiki/Scheme#%D0%92%D0%B2%D0%BE%D0%B4-%D0%B2%D1%8B%D0%B2%D0%BE%D0%B4
;; https://stackoverflow.com/questions/43347366/how-to-write-to-a-file-in-append-mode-scheme-r5rs

; (use-modules (ice-9 readline))
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


;; (with-open-input-port)
;; (with-input-from-file)
; (define (lazy-spit name str))
; (define (lazy-slurp name))