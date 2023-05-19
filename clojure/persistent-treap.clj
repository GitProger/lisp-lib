(deftype tnode [left right x y]
  Object
  (toString [this]
    (str "(" x ", " y ") <" (if (nil? left) "nil" "ptr") "," (if (nil? right) "nil" "ptr") ">")))

(defn make-node [x y] (tnode. nil nil x y))
(defn make-rnd-node [x] (tnode. nil nil x (rand-int 1e7)))

(defn correct? [t]
  (and
    (or (nil? t) (instance? tnode t))
    (or (nil? (.left t)) (instance? tnode (.left t)))
    (or (nil? (.right t)) (instance? tnode (.right t)))))

(defn find-node [tree v]
  (if (nil? tree)
    nil
    (let [l (.left tree) r (.right tree) x (.x tree)]
      (cond
        (< v x) (if (nil? l) nil (recur l v))
        (> v x) (if (nil? r) nil (recur r v))
        :else tree))))

(defn find-min [tree]
  (if (nil? (.left tree))
    tree
    (recur (.left tree))))

(defn find-max [tree]
  (if (nil? (.right tree))
    tree
    (recur (.right tree))))

(defn split-tree [tree s]
  (if (nil? tree)
    [nil nil]
    (let [l (.left tree) r (.right tree) x (.x tree) y (.y tree)]
      (let [[r1 r2] (split-tree (if (<= s x) l r) s)]
        (if (<= s x)
          [r1 (tnode. r2 r x y)]
          [(tnode. l r1 x y) r2])))))

(defn merge-trees [t1 t2]
  (cond
    (nil? t1) t2
    (nil? t2) t1
    :else (if (<= (.y t1) (.y t2))
            (tnode. (.left t1) (merge-trees (.right t1) t2) (.x t1) (.y t1))
            (tnode. (merge-trees t1 (.left t2)) (.right t2) (.x t2) (.y t2)))))

(defn add-node [tree new]
  (cond
    (some? (find-node tree (.x new))) tree
    (nil? tree) new
    :else (if (< (.y new) (.y tree))
            (let [[r1 r2] (split-tree tree (.x new))]
              (tnode. r1 r2 (.x new) (.y new)))
            (add-node (if (< (.x new) (.x tree)) (.left tree) (.right tree)) new))))

(defn add-value [tree v]
  (if (some? (find-node tree v))
    tree
    (let [[r1 r2] (split-tree tree v)]
      (merge-trees (merge-trees r1 (make-rnd-node v)) r2))))

(defn remove-value [tree v]
  (if (nil? tree)
    tree
    (let [l (.left tree) r (.right tree) x (.x tree)]
      (if (= x v)
        (merge-trees l r)
        (remove-value (if (< v x) l r) v)))))

