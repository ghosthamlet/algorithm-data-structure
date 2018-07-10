(ns algorithm-data-structure.algorithms.math.pascal-triangle)

(defn run [line-number]
  (loop [current-line [(bigint 1)]
         num-index 1]
    (if (= num-index (inc line-number))
      current-line
      (recur (conj current-line
                   (/ (* (current-line (dec num-index))
                         (inc (- line-number num-index)))
                      num-index))
             (inc num-index)))))
