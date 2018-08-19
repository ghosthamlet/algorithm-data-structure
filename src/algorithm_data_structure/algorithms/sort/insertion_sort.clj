(ns algorithm-data-structure.algorithms.sort.insertion-sort)

(defn run
  [original-array
   & {{:keys [visiting-callback]} :callbacks
      {:keys [less-then]} :comparator}]
  (reduce (fn [array i]
            (visiting-callback (nth array i))
            (loop [current-index i
                   array array]
              (let [last-index (dec current-index)
                    last-element (nth array last-index)]
                (if (and ((comp not= nil?) last-element)
                         (less-then (nth array current-index)
                                    last-element))
                  (do
                    (visiting-callback last-element)
                    (recur last-index
                           (assoc array
                                  last-index (nth array current-index)
                                  current-index last-element)))
                  array))))
          original-array (count original-array)))
