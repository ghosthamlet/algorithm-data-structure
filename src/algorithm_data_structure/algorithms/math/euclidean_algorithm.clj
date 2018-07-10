(ns algorithm-data-structure.algorithms.math.euclidean-algorithm)

(defn run [original-a original-b]
  (let [a (Math/abs original-a)
        b (Math/abs original-b)]
    (cond
      (and (zero? a) (zero? b)) nil
      (and (zero? a) (not (zero? b))) b
      (and (not (zero? a)) (zero? b)) a
      (> a b) (recur (mod a b) b)
      :else (recur (mod b a) a))))
