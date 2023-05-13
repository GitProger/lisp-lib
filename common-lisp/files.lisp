
(defun spit (name str)
  (with-open-file (file name
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~a" str)))

(defun slurp (filename &optional (is-list nil))
  (let ((contents (with-open-file (stream filename)
                    (loop for line = (read-line stream nil)
                          while line
                          collect line))))
    (if is-list 
      contents
      (format nil "~{~A~^~%~}" contents))))
