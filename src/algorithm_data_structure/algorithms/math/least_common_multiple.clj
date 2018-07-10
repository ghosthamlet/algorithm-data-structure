(ns algorithm-data-structure.algorithms.math.least-common-multiple
  (:require [algorithm-data-structure.algorithms.math.euclidean-algorithm :as euclidean-algorithm]))

(defn run [a b]
  (if (or (zero? a) (zero? b))
    0
    (/ (Math/abs (* a b)) (euclidean-algorithm/run a b))))
