(ns algorithm-data-structure.data-structures.min-heap)

(def compare)
(def less-then)

(defn but-nth [xs pos]
  (vec (case pos
         :first (next xs)
         :last (butlast xs)
         (concat (subvec xs 0 pos) (subvec xs (inc pos))))))

(defn move [xs from to]
  (assoc (but-nth xs from)
         to (case from
              :first (first xs)
              :last (last xs)
              (nth xs from))))

(defn get-left-child-index [parent-index]
  (+ (* 2 parent-index) 1))

(defn get-right-child-index [parent-index]
  (+ (* 2 parent-index) 2))

(defn get-parent-index [child-index]
  (Math/floor (/ (dec child-index) 2)))

(defn has-parent [child-index]
  (>= (get-parent-index child-index) 0))

(defn has-left-child [heap-container parent-index]
  (< (get-left-child-index parent-index) (count heap-container)))

(defn has-right-child [heap-container parent-index]
  (< (get-right-child-index parent-index) (count heap-container)))

(defn left-child [heap-container parent-index]
  (nth heap-container (get-left-child-index parent-index)))

(defn right-child [heap-container parent-index]
  (nth heap-container (get-right-child-index parent-index)))

(defn parent [heap-container child-index]
  (nth heap-container (get-parent-index child-index)))

(defn swap [heap-container index-one index-two]
  (assoc heap-container
         index-one (nth heap-container index-two)
         index-two (nth heap-container index-one)))

(defn heapify-up
  ([heap-container]
   (heapify-up heap-container (dec (count heap-container))))
  ([heap-container custom-start-index]
   (if (and (has-parent custom-start-index)
            (less-then (nth heap-container custom-start-index)
                       (parent custom-start-index)))
     (recur (swap heap-container
                  custom-start-index
                  (get-parent-index custom-start-index))
            (get-parent-index custom-start-index))
     heap-container)))

(defn heapify-down
  ([heap-container]
   (heapify-down heap-container 0))
  ([heap-container custom-start-index]
   (if (has-left-child custom-start-index)
     (let [next-index (if (and (has-right-child heap-container custom-start-index)
                               (less-then (right-child custom-start-index)
                                          (left-child custom-start-index)))
                        (get-right-child-index heap-container custom-start-index)
                        (get-left-child-index heap-container custom-start-index))]
       (if (less-then (nth heap-container custom-start-index)
                      (nth heap-container next-index))
         heap-container
         (recur (swap heap-container custom-start-index next-index)
                next-index)))
     heap-container)))

(defn peek [heap-container]
  (when (pos? (count heap-container))
    (first heap-container)))

(defn poll [heap-container]
  (case (count heap-container)
    0 [heap-container nil]
    1 ((juxt butlast last) heap-container)
    [(heapify-down (move heap-container :last 0))
     (first heap-container)]))

(defn add [heap-container item]
  (heapify-up (conj heap-container item)))

(defn find*
  ([heap-container]
   (find* heap-container compare))
  ([heap-container custom-comparator]
   (let [len (count heap-container)]
     (loop [item-index 0
            found-item-indices []]
       (if (= item-index len)
         found-item-indices
         (recur (inc item-index)
                (if (custom-comparator item
                                       (nth heap-container item-index))
                  (conj found-item-indices item-index)
                  found-item-indices)))))))

(defn remove*
  ([heap-container item]
   (remove* heap-container item compare))
  ([heap-container item custom-comparator]
   (let [number-of-items-to-remove (find* heap-container item)]
     (loop [iteration 0
            heap-container heap-container]
       (if (= iteration number-of-items-to-remove)
         heap-container
         (recur (inc iteration)
                (let [index-to-remove (last (find* heap-container item))]
                  (if (->> heap-container count dec (= index-to-remove))
                    (butlast heap-container)
                    (let [heap-container (move heap-container :last index-to-remove)
                          parent-item (when (has-parent heap-container index-to-remove)
                                        (parent heap-container index-to-remove))
                          left-child-item (when (has-left-child heap-container
                                                                index-to-remove)
                                            (left-child heap-container index-to-remove))]
                      (if (->> index-to-remove
                               (nth heap-container)
                               (less-then parent-item)
                               (or parent-item)
                               (and left-child-item))
                        (heapify-down heap-container index-to-remove)
                        (heapify-up heap-container index-to-remove)))))))))))

(defn empty? [heap-container]
  (zero? (count heap-container)))

(defn ->string [heap-container]
  (str heap-container))
