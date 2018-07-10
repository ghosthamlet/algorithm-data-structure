(ns algorithm-data-structure.algorithms.math.factorial)

(defn run [number]
  (reduce * 1 (range 1 (inc number))))
