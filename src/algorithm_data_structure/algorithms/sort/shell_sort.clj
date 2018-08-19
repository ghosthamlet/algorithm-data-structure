(ns algorithm-data-structure.algorithms.sort.shell-sort)

(defn run
  [original-array
   & {{:keys [visiting-callback]} :callbacks
      {:keys [less-then]} :comparator}]
  (let [alen (count array)
        array (atom original-array)
        gap (atom (Math/floor (/ alen 2)))]
    (while (pos? @gap)
      (dotimes [i (- alen @gap 1)]
        (let [current-index (atom i)
              gap-shifted-index (atom (+ i @gap))
              current-element (nth @array @current-index)
              gap-shifted-element (nth @array @gap-shifted-index)]
          (while (>= @current-index 0)
            (visiting-callback current-element)
            (when (less-then gap-shifted-element
                             current-element)
              (swap! array assoc
                     @current-index gap-shifted-element
                     @gap-shifted-index current-element))
            (reset! gap-shifted-index @current-index)
            (swap! current-index - @gap))))
      (swap! gap #(-> % (/ 2) Math/floor)))
    @array))
