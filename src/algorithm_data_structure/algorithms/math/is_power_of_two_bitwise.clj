(ns algorithm-data-structure.algorithms.math.is-power-of-two-bitwise)

(defn run [number]
  (if (< number 1)
    false
    (zero? (bit-and number (dec number)))))
