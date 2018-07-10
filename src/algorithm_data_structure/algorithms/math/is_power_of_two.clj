(ns algorithm-data-structure.algorithms.math.is-power-of-two)

(defn run [number]
  (if (< number 1)
    false
    (loop [divided-number number]
      (if (= 1 divided-number)
        true
        (if (not= 0 (mod divided-number 2))
          false
          (recur (/ divided-number 2)))))))
