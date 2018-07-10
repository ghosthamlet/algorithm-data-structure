(ns algorithm-data-structure.algorithms.math.bits.clear-bit)

(defn run [number bit-position]
  (let [mask (bit-not (bit-shift-left 1 bit-position))]
    (bit-and number mask)))
