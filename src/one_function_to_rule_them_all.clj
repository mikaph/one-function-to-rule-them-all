(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn
  str-cat
  [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str % " " %2) a-seq)))

(defn
  my-interpose
  [x a-seq]
  (drop 1 (reduce #(conj % x %2) [] a-seq)))


(defn
  my-count
  [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))


(defn
  my-reverse
  [a-seq]
  (reduce #(cons %2 %) [] a-seq))

(defn
  min-max-element
  [a-seq]
  (let [helper (fn [minmax y] (cond
                                (and (> (first minmax) y) (< (second minmax) y)) (vector y y)
                                (> (first minmax) y) (vector y (second minmax))
                                (< (second minmax) y) (vector (first minmax) y)
                                :else minmax))]
  (reduce helper [Double/POSITIVE_INFINITY 0] a-seq)))


(defn
  insert
  [sorted-seq n]
  (loop [s-seq sorted-seq
         inserted false
         result '()]
    (cond
      (and (empty? s-seq) inserted) result
      (empty? s-seq) (concat result [n])
      inserted (recur (rest s-seq) true (concat result (vector (first s-seq))))
      (>= (first s-seq) n) (recur (rest s-seq) true (concat result [n] (vector (first s-seq))))
      :else (recur (rest s-seq) false (concat result (vector (first s-seq)))))))



(defn
  insertion-sort
  [a-seq]
  (reduce insert '() a-seq))

(defn
  parity
  [a-seq]
  (let [toggle (fn [a-set x] (if (contains? a-set x)
                               (disj a-set x)
                               (conj a-set x)))]
  (reduce toggle #{} a-seq)))


(defn
  minus
  ([x] (- x))
  ([x y] (- x y)))


(defn
  count-params
  [& more]
  (count more))

(defn
  my-*
  ([] 1)
  ([x] (x))
  ([x & more] (reduce * x more)))


(defn pred-and
  ([] (fn [y] (identity true)))
  ([& more] (fn [y] (reduce #(and % (%2 y)) true more))))

(defn my-map [f a-seq]
  [:-])
