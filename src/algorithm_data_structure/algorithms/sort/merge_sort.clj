(ns algorithm-data-structure.algorithms.sort.merge-sort)

(defn run
  [original-array
   & {{:keys [visiting-callback]} :callbacks
      {:keys [less-then-or-equal]} :comparator}]
  (visiting-callback nil)
  (let [olen (count original-array)]
    (if (<= olen 1)
      original-array
      (let [middle-index (Math/floor (/ olen 2))
            left-array (take middle-index original-array)
            right-array (drop middle-index original-array)
            merge-sorted-arrays (fn [left-array right-array]
                                  (let [[left-array right-array sorted-array]
                                        (loop [left-array left-array
                                               right-array right-array
                                               sorted-array []]
                                          (if (or (empty? left-array) (empty? right-array))
                                            [left-array right-array sorted-array]
                                            (let [left-first (first left-array)
                                                  right-first (first right-array)
                                                  minimum-element (if (less-then-or-equal left-first right-first)
                                                                    left-first
                                                                    right-first)]
                                              (visiting-callback minimum-element)
                                              (recur (rest left-array)
                                                     (rest right-array)
                                                     (conj sorted-array minimum-element)))))]
                                    (concat sorted-array
                                            (when (> (count left-array) 0)
                                              left-array)
                                            (when (> (count right-array) 0)
                                              right-array))))]
        (merge-sorted-arrays (run left-array) (run right-array))))))
