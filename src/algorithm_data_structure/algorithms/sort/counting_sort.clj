(ns algorithm-data-structure.algorithms.sort.counting-sort)

(defn run
  [original-array
   & {:keys [biggest-element]
      :or {biggest-element 0}
      {:keys [visiting-callback]} :callbacks
      {:keys [less-then greater-than]} :comparator}]
  (let [olen (count original-array)
        detected-biggest-element (atom biggest-element)]
    (when (not @detected-biggest-element)
      (doseq [element original-array]
        (visiting-callback element)
        (when (greater-than element @detected-biggest-element)
          (reset! detected-biggest-element element))))
    (let [buckets (atom (repeat (inc @detected-biggest-element) 0))
          sorted-array (atom (repeat olen nil))]
      (doseq [element original-array]
        (visiting-callback element)
        (swap! buckets update element inc))
      (doseq [bucket-index (range 1 (count buckets))]
        (swap! buckets update bucket-index
               #(+ % (nth @buckets (dec bucket-index)))))
      (swap! buckets (comp vec drop-last))
      (swap! buckets #((comp vec cons) 0 %))
      (doseq [element-index (range olen)
              :let [element (nth original-array element-index)]]
        (visiting-callback element)
        (swap! sorted-array assoc (nth @buckets element) element)
        (swap! buckets update element inc))
      @sorted-array)))
