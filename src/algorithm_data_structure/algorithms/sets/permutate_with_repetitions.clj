(ns algorithm-data-structure.algorithms.sets.permutate-with-repetitions)

(defn run
  ([permutation-options]
   (run permutation-options (count permutation-options)))
  ([permutation-options permutation-length]
   (if (= 1 permutation-length)
     (map (fn [x] [x]) permutation-options)
     (reduce (fn [permutations current-option]
               (->> permutation-length
                    dec
                    (run permutation-options)
                    (map #(concat [current-option] %))
                    (concat permutations)))
             [] permutation-options))))

