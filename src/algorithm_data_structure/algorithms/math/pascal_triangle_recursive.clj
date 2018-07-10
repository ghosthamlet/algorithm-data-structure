(ns algorithm-data-structure.algorithms.math.pascal-triangle-recursive)

(defn run [line-number]
  (if (zero? line-number)
    [(bigint 1)]
    (let [current-line-size (inc line-number)
          previous-line-size line-number
          previous-line (run (dec line-number))]
      (loop [num-index 0
             current-line []]
        (if (= num-index current-line-size)
          current-line
          (recur (inc num-index)
                 (let [left-coefficient (if (>= (dec num-index) 0)
                                          (previous-line (dec num-index))
                                          0)
                       right-coefficient (if (< num-index previous-line-size)
                                           (previous-line num-index)
                                           0)]
                   (conj current-line (+ left-coefficient right-coefficient)))))))))
