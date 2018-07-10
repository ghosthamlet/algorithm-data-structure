(ns algorithm-data-structure.algorithms.search.binary-search
  (:require [algorithm-data-structure.util :refer :all]))

(defn run [sorted-array seek-element
           & {:keys [equal less-then]
              :or {equal (compare-value :equal)
                   less-then (compare-value :less-then)}}]
  (loop [start-index 0
         end-index (dec (count sorted-array))]
    (if (> start-index end-index)
      -1
      (let [middle-index (+ start-index
                            (Math/floor (/ (- end-index start-index) 2)))
            middle-element (sorted-array middle-index)]
        (if (equal middle-element seek-element)
          middle-index
          (let [[start-index end-index] (if (less-then middle-element seek-element)
                                          [(inc middle-index) end-index]
                                          [start-index (dec middle-index)])]
            (recur start-index end-index)))))))
