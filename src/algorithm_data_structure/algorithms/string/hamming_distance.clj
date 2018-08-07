(ns algorithm-data-structure.algorithms.string.hamming-distance)

(defn run [a b]
  (if (not= (count a) (count b))
    (throw (Exception. "Strings must be of the same length"))
    (->> b (map = a) (filter false?) count)))
