(ns algorithm-data-structure.algorithms.math.bits.set-bit)

(defn run [number bit-position]
  (bit-or number (bit-shift-left 1 bit-position)))
