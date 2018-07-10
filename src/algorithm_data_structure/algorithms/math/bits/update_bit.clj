(ns algorithm-data-structure.algorithms.math.bits.update-bit)

(defn run [number bit-position bit-value]
  (let [bit-value-normalized (if bit-value 1 0)
        clear-mask (bit-not (bit-shift-left 1 bit-position))]
    (bit-or (bit-and number clear-mask)
            (bit-shift-left bit-value-normalized bit-position))))
