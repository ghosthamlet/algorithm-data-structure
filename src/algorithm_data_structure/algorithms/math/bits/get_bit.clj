(ns algorithm-data-structure.algorithms.math.bits.get-bit)

(defn run [number bit-position]
  (bit-and (bit-shift-right number bit-position) 1))
