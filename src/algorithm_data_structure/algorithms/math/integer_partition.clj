(ns algorithm-data-structure.algorithms.math.integer-partition)

(defn run [number]
  (let [ns (range (inc number))
        partition-matrix (-> nil constantly (map ns) constantly (map ns))
        partition-matrix (assoc partition-matrix
                                0 (-> 0 constantly (map ns) (assoc 0 nil)))
        partition-matrix (map #(assoc % 0 1) partition-matrix)
        f (fn [summand-index number-index pm]
            (if (= number-index ns)
              pm
              (recur summand-index
                     (inc number-index)
                     (if (> summand-index number-index)
                       (assoc-in pm
                                 [summand-index number-index]
                                 (get-in pm [(dec summand-index) number-index]))
                       (let [combos-without-summand (get-in pm
                                                            [(dec summand-index) number-index])
                             combos-with-summand (get-in pm
                                                         [summand-index (- number-index summand-index)])]
                         (assoc-in pm
                                   [summand-index number-index]
                                   (+ combos-without-summand combos-with-summand)))))))]
    (loop [summand-index 1
           partition-matrix partition-matrix]
      (if (= summand-index ns)
        (get-in partition-matrix [number number])
        (recur (inc summand-index)
               (f summand-index 1 partition-matrix))))))
