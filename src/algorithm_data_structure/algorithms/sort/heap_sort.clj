(ns algorithm-data-structure.algorithms.sort.heap-sort
  (:require [algorithm-data-structure.data-structures.min-heap :as mh]))

(defn run
  [original-array
   & {{:keys [visiting-callback compare-callback]} :callbacks
      {:keys [less-then greater-than]} :comparator}]
  (loop [sorted-array []
         min-heap (reduce (fn [acc x]
                            (visiting-callback x)
                            (mh/add acc x))
                          (mh/create compare-callback) original-array)]
    (if (mh/empty? min-heap)
      sorted-array
      (let [[min-heap next-min-element] (mh/poll min-heap)]
        (visiting-callback next-min-element)
        (recur (conj sorted-array next-min-element)
               min-heap)))))
