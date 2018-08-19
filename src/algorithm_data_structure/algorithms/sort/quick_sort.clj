(ns algorithm-data-structure.algorithms.sort.quick-sort)

(defn run
  [original-array
   & {{:keys [visiting-callback]} :callbacks
      {:keys [equal less-then]} :comparator}]
  (let [olen (count original-array)
        array original-array]
    (if (<= olen 1)
      original-array
      (let [pivot-element (first array)
            [left-array-sorted right-array-sorted center-array]
            (loop [array (rest original-array)
                   left-array []
                   right-array []
                   center-array [pivot-element]]
              (if (empty? array)
                [(run left-array) (run right-array) center-array]
                (let [current-element (first array)
                      eq (equal current-element pivot-element)
                      lt (less-then current-element pivot-element)
                      center-array (if eq
                                     (conj center-array current-element)
                                     center-array)
                      left-array (if lt
                                   (conj left-array current-element)
                                   left-array)
                      right-array (if-not (or eq lt)
                                    (conj right-array current-element)
                                    right-array)]
                  (visiting-callback current-element)
                  (recur (rest array)
                         left-array
                         right-array
                         center-array))))]
        (concat left-array-sorted center-array right-array-sorted)))))
