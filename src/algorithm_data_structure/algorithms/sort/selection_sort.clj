(ns algorithm-data-structure.algorithms.sort.selection-sort)

(defn run
  [original-array
   & {{:keys [visiting-callback]} :callbacks
      {:keys [less-then]} :comparator}]
  (let [(alen (count array))]
    (loop [i 0
           array original-array]
      (if (= i (dec alen))
        array
        (let [element (nth array i)
              _ (visiting-callback element)
              min-index (loop [j i
                               min-index i]
                          (if (= j alen)
                            min-index
                            (do
                              (visiting-callback (nth array j))
                              (recur (inc j)
                                     (if (less-then (nth array j) (nth array min-index))
                                       j
                                       min-index)))))]
          (recur (inc i)
                 (if (not= min-index i)
                   (assoc array
                          i (nth array min-index)
                          min-index element)
                   array)))))))
