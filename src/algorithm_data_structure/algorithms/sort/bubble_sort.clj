(ns algorithm-data-structure.algorithms.sort.bubble-sort)

(defn run [original-array
           & {{:keys [visiting-callback]} :callbacks
              {:keys [less-then]} :comparator}]
  (let [swapped (atom false)
        array (atom original-array)
        alen (count array)]
    (doseq [i (range 1 alen)
            ;; have to use seperate doseq for visiting-callback
            ;; j (range 0 alen)
            ;; add = i 1 to prevent break in first step
            :when (or (= i 1) @swapped)]
      (reset! swapped false)
      (visiting-callback (nth @array i))
      (doseq [j (range 0 alen)]
        (visiting-callback (nth @array j))
        (when (less-then (nth @array (inc j)) (nth @array j))
          (let [tmp (nth @array (inc j))]
            (swap! array assoc (inc j) (nth @array j))
            (swap! array assoc j tmp)
            (reset! swapped true)))))
    @array))
