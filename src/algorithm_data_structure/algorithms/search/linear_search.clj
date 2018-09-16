(ns algorithm-data-structure.algorithms.search.linear-search
  (:require [algorithm-data-structure.util :refer :all]))

(defn run [array seek-element
           & {:keys [equal less-then]
              :or {equal (compare-value :equal)}}]
  (loop [index 0
         found-indices []]
    (if (= index (count array))
      found-indices
      (recur (inc index)
             (if (equal (array index) seek-element)
               (conj found-indices index)
               found-indices)))))
