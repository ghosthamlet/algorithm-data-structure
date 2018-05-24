(ns algorithm-data-structure.util)

(defn but-nth [xs pos]
  (vec (case pos
         :first (next xs)
         :last (butlast xs)
         (concat (subvec xs 0 pos) (subvec xs (inc pos))))))

(defn move [xs from to]
  (assoc (but-nth xs from)
         to (case from
              :first (first xs)
              :last (last xs)
              (nth xs from))))
