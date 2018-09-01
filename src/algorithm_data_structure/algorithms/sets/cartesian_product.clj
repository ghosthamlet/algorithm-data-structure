(ns algorithm-data-structure.algorithms.sets.cartesian-product)

(defn run [set-a set-b]
  (when (and set-a set-b (seq set-a) (seq set-b))
    (for [a set-a
          b set-b]
      [a b])))
