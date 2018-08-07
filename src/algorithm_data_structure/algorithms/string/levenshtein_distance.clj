(ns algorithm-data-structure.algorithms.string.levenshtein-distance)

(defn run [a b]
  (let [alen (count a)
        blen (count b)
        distance-matrix (map (fn [_] (repeat (inc alen) nil))
                             (repeat (inc blen) nil))
        calc (fn [j]
               (fn [i x]
                 (if (zero? i)
                   j
                   (let [indicator (if (= (nth a (dec i)) (nth b (dec j))) 0 1)]
                     (Math/min
                      (inc (get-in distance-matrix [j (dec i)]))
                      (inc (get-in distance-matrix [(dec j) i]))
                      (+ indicator (get-in distance-matrix [(dec j) (dec i)])))))))]
    (get-in (map-indexed (fn [j y]
                           (if (zero? j) y (map-indexed (calc j) y)))
                         (->> alen inc range (assoc distance-matrix 0)))
            [blen alen])))
