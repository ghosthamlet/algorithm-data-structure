(ns algorithm-data-structure.algorithms.sets.dp-maximum-subarray)

(defn run [input-array]
  (let [all-negative (atom true)
        highest-element-value (atom nil)]
    (doseq [item input-array]
      (when (>= item 0)
        (reset! all-negative false))
      (when (or (nil? @highest-element-value)
                (< @highest-element-value item))
        (reset! highest-element-value item)))
    (if (and @all-negative
             (not= @highest-element-value nil))
      [@highest-element-value]
      (let [max-sum (atom 0)
            max-sub-array (atom [])
            current-sum (atom 0)
            current-sub-array (atom [])]
        (doseq [item input-array]
          (swap! current-sum + item)
          (if (neg? @current-sum)
            (do
              (reset! current-sum 0)
              (reset! current-sub-array []))
            (do
              (swap! current-sub-array conj item)
              (when (> @current-sum @max-sum)
                (reset! max-sum @current-sum)
                (reset! max-sub-array @current-sub-array)))))
        @max-sub-array))))
