(ns algorithm-data-structure.algorithms.sets.combination-sum)

(defn- combination-sum-recursive
  [candidates remaining-sum
   & {:keys [final-combinations
             current-combination
             start-from]
      :or {final-combinations []
           current-combination []
           start-from 0}}]
  (cond
    (neg? remaining-sum) final-combinations
    (zero? remaining-sum) (conj final-combinations current-combination)
    :else
    (loop [candidate-index start-from
           final-combinations final-combinations]
      (if (= candidate-index (count candidates))
        final-combinations
        (recur (inc candidate-index)
               (let [current-candidate (candidates candidate-index)]
                 (combination-sum-recursive candidates
                                            (- remaining-sum current-candidate)
                                            final-combinations
                                            (conj current-combination current-candidate)
                                            candidate-index)))))))

(defn run [candidates target]
  (combination-sum-recursive candidates target))
