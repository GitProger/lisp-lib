(defun xread (stream) (read stream nil #\newline t))

(set-macro-character #\] (constantly '])) ; '] => #\]
(set-macro-character #\[ ; vector-reader
  #'(lambda (stream char)
    (declare (ignore char))
    (let ((vec (make-array 1 :adjustable t :fill-pointer 0)))
      (do ((tok (xread stream) (xread stream)))
        ((eq tok '])) 
        (vector-push-extend tok vec))
      vec)))


(set-macro-character #\} (constantly '}))
(set-macro-character #\{  ; hashmap-reader
  #'(lambda (stream char)
    (declare (ignore char))
    (let ((hash (make-hash-table)) (key nil))
      (do ((tok (xread stream) (xread stream)))
        ((eq tok '})) 
        (if (null key)
          (setf key tok)
          (progn 
            (if (multiple-value-bind (_ exists) (gethash key hash) (declare (ignore _)) exists)
              (cerror "Key duplicate" 'parse-error)) 
            (setf (gethash key hash) tok)
            (setf key nil))))
      (if key (cerror "Odd element count" 'parse-error))
      hash)))



(defun hash-keys (ht)
  (let ((keys nil))
    (maphash 
      #'(lambda (k v)
        (declare (ignore v))
        (push k keys))
      ht)
    (reverse keys)))

(defun hash-pairs (ht)
  (let ((pairs nil))
    (maphash 
      #'(lambda (k v)
        (push (list k v) pairs))
      ht)
    (reverse pairs)))

(load "functional")

(defun join (sep strs)
  (apply #'str (interpose sep strs)))

(defun hash->str (h)
  (str "{" 
    (join ", "
      (mapcar 
          #'(lambda (a) (join " " (mapcar #'write-to-string a)))
          (hash-pairs h))) "}"))

