(ns algorithm-data-structure.algorithms.sets.combine-without-repetitions)

(defn run [combo-options combo-length]
  (if (= 1 combo-length)
    (map (fn [x] [x]) combo-options)
    (loop [i 0
           combos []]
      (if (= i (count combo-options))
        combos
        (recur (inc i)
               (reduce #(conj %1 (concat [(nth combo-options i)] %2))
                       combos
                       (run (drop (inc i) combo-options) (dec combo-length))))))))
