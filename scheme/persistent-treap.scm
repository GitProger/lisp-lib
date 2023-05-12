;(load "functional.scm")
;(load "lib.scm")
;(load "classes.scm")
(defrecord tnode (left right x y))

(define nil '())
(define nil? null?)
(define some? (comp not null?))

(defn make-node [x y] (tnode nil nil x y))
(defn make-rnd-node [x] (tnode nil nil x (random (inexact->exact 1e7))))

(defn find-node [tree v]
  (if (nil? tree)
    nil
    (let ([l (tree 'left)] [r (tree 'right)] [x (tree 'x)])
      (cond
        [(< v x) (if (nil? l) nil (find-node l v))]
        [(> v x) (if (nil? r) nil (find-node r v))]
        [else tree]))))


(defn find-min [tree]
  (if (nil? (tree 'left))
    tree
    (find-min (tree 'left))))

(defn find-max [tree]
  (if (nil? (tree 'right))
    tree
    (find-max (tree 'right))))

(defn split-tree [tree s]
  (if (nil? tree)
    [cons nil nil]
    (let ([l (tree 'left)] [r (tree 'right)] [x (tree 'x)] [y (tree 'y)])
      (let* ([p (split-tree (if (<= s x) l r) s)] [r1 (car p)] [r2 (cdr p)])
        (if (<= s x)
          [cons r1 (tnode r2 r x y)]
          [cons (tnode l r1 x y) r2])))))

(defn merge-trees [t1 t2]
  (cond
    [(nil? t1) t2]
    [(nil? t2) t1]
    [else (if (<= (t1 'y) (t2 'y))
            (tnode (t1 'left) (merge-trees (t1 'right) t2) (t1 'x) (t1 'y))
            (tnode (merge-trees t1 (t2 'left)) (t2 'right) (t2 'x) (t2 'y)))]))

(defn add-node [tree new]
  (cond
    [(some? (find-node tree (new 'x))) tree]
    [(nil? tree) new]
    [else (if (< (new 'y) (tree 'y))
            (let* ([p (split-tree tree (new 'x))] [r1 (car p)] [r2 (cdr p)])
              (tnode r1 r2 (new 'x) (new 'y)))
            (add-node (if (< (new 'x) (tree 'x)) (tree 'left) (tree 'right)) new))]))

(defn add-value [tree v]
  (if (some? (find-node tree v))
    tree
    (let* ([p (split-tree tree v)] [r1 (car p)] [r2 (cdr p)])
      (merge-trees (merge-trees r1 (make-rnd-node v)) r2))))

(defn remove-value [tree v]
  (if (nil? tree)
    tree
    (let ([l (tree 'left)] [r (tree 'right)] [x (tree 'x)])
      (if (= x v)
        (merge-trees l r)
        (remove-value (if (< v x) l r) v)))))
