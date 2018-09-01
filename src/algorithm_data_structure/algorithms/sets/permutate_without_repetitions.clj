(ns algorithm-data-structure.algorithms.sets.permutate-without-repetitions)

(defn run [permutation-options]
  (if (= 1 (count permutation-options))
    [permutation-options]
    (reduce (fn [permutations smaller-permutation]
              (->> [smaller-permutation nil]
                   (map-indexed (fn [position-index _]
                                  (concat (take position-index
                                                smaller-permutation)
                                          [(first permutation-options)]
                                          (drop position-index
                                                smaller-permutation))))
                   (concat permutations)))
            [] (run (rest permutation-options)))))
