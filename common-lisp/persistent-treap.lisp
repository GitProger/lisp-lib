;functions returns tnode (a closure)
;(load "functional.scm")
;(load "lib.scm")
;(load "classes.scm")
(defrecord tnode (left right x y))


(defn make-node (x y) (tnode nil nil x y))
(defn make-rnd-node (x) (tnode nil nil x (random (floor 1e7))))

(defn find-node (tree v)
  (if (null tree)
    nil
    (let ((l (getfield tree left)) (r (getfield tree right)) (x (getfield tree x)))
      (cond
        ((< v x) (if (null l) nil (find-node l v)))
        ((> v x) (if (null r) nil (find-node r v)))
        (:else tree)))))


(defn find-min (tree)
  (if (null (getfield tree left))
    tree
    (find-min (getfield tree left))))

(defn find-max (tree)
  (if (null (getfield tree right))
    tree
    (find-max (getfield tree right))))

(defn split-tree (tree s)
  (if (null tree)
    (cons nil nil)
    (let ((l (getfield tree left)) (r (getfield tree right)) (x (getfield tree x)) (y (getfield tree y)))
      (let* ((p (split-tree (if (<= s x) l r) s)) (r1 (car p)) (r2 (cdr p)))
        (if (<= s x)
          (cons r1 (tnode r2 r x y))
          (cons (tnode l r1 x y) r2))))))

(defn merge-trees (t1 t2)
  (cond
    ((null t1) t2)
    ((null t2) t1)
    (:else (if (<= (getfield t1 y) (getfield t2 y))
             (tnode (getfield t1 left) (merge-trees (getfield t1 right) t2) (getfield t1 x) (getfield t1 y))
             (tnode (merge-trees t1 (getfield t2 left)) (getfield t2 right) (getfield t2 x) (getfield t2 y))))))

(defn add-node (tree new)
  (cond
    ((not (null (find-node tree (getfield new x)))) tree)
    ((null tree) new)
    (:else (if (< (getfield new y) (getfield tree y))
             (let* ((p (split-tree tree (getfield new x))) (r1 (car p)) (r2 (cdr p)))
               (tnode r1 r2 (getfield new x) (getfield new y)))
             (add-node (if (< (getfield new x) (getfield tree x)) (getfield tree left) (getfield tree right)) new)))))

(defn add-value (tree v)
  (if (not (null (find-node tree v)))
    tree
    (let* ((p (split-tree tree v)) (r1 (car p)) (r2 (cdr p)))
      (merge-trees (merge-trees r1 (make-rnd-node v)) r2))))

(defn remove-value (tree v)
  (if (null tree)
    tree
    (let ((l (getfield tree left)) (r (getfield tree right)) (x (getfield tree x)))
      (if (= x v)
        (merge-trees l r)
        (remove-value (if (< v x) l r) v)))))
