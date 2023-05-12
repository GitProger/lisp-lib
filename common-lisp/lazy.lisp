
(defmacro delay (form)
  `(lambda () ,form))

(defun force (form)
  (when form (funcall form)))

