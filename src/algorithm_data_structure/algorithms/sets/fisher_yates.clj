(ns algorithm-data-structure.algorithms.sets.fisher-yates)

(defn run [original-array]
  (loop [i (dec (count original-array))
         array original-array]
    (if (zero? i)
      array
      (recur (dec i)
             (let [random-index (Math/floor (* (rand) (inc i)))]
               (assoc array
                      i (nth array random-index)
                      random-index (nth array i)))))))
