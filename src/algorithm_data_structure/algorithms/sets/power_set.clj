(ns algorithm-data-structure.algorithms.sets.power-set)

(defn run [original-set]
  (let [olen (count original-set)]
    (reduce (fn [sub-sets combination-index]
              (conj sub-sets
                    (reduce (fn [sub-set set-element-index]
                              (if (zero? (bit-and combination-index
                                                  (bit-shift-left set-element-index 1)))
                                sub-set
                                (conj sub-set
                                      (original-set set-element-index))))
                            [] (range olen))))
            [] (range (Math/pow olen 2)))))
