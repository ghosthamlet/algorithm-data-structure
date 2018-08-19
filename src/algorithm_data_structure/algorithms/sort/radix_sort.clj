(ns algorithm-data-structure.algorithms.sort.radix-sort)

(def base-char-code 97)
(def number-of-possible-digits 10)
(def engish-alphabet-length 26)

(declare place-elements-in-number-buckets
         place-elements-in-character-buckets
         get-char-code-of-element-at-index
         determine-num-passes
         get-length-of-longest-element
         array-of-numbers?
         create-buckets)

(defn run
  [original-array
   & {{:keys [visiting-callback]} :callbacks
      {:keys [less-then-or-equal]} :comparator}]
  (let [is-array-of-numbers (array-of-numbers? original-array)
        num-passes (determine-num-passes original-array)]
    (loop [current-index 0
           sorted-array original-array]
      (if (= current-index num-passes)
        sorted-array
        (recur (inc current-index)
               (apply concat
                      (if is-array-of-numbers
                        (place-elements-in-number-buckets sorted-array current-index visiting-callback)
                        (place-elements-in-character-buckets sorted-array current-index num-passes visiting-callback))))))))

(defn place-elements-in-number-buckets [array index visiting-callback]
  (let [modded (Math/pow 10 (inc index))
        divided (Math/pow 10 index)]
    (loop [array array
           buckets (create-buckets number-of-possible-digits)]
      (if (empty? array)
        buckets
        (let [element (first array)]
          (visiting-callback element)
          (recur (rest array)
                 (update buckets (if (< element divided)
                                   0
                                   (Math/floor (/ (mod element modded)
                                                  divided)))
                         conj element)))))))

(defn place-elements-in-character-buckets [array index num-passes visiting-callback]
  (loop [array array
         buckets (create-buckets engish-alphabet-length)]
    (if (empty? array)
      buckets
      (let [element (first array)]
        (visiting-callback element)
        (recur (rest array)
               (update buckets
                       (get-char-code-of-element-at-index element index num-passes)
                       conj element))))))

(def get-char-code-of-element-at-index [element index num-passes]
  (let [elen (count element)]
    (if (> (- num-passes index) elen)
      (dec engish-alphabet-length)
      (- (int (nth (.toLowerCase element)
                   (if (> index (dec elen))
                     0
                     (- elen index 1))))
         base-char-code))))

(def determine-num-passes [array]
  (get-length-of-longest-element array))

(def get-length-of-longest-element [array]
  (if (array-of-numbers? array)
    (->> array (apply max) Math/log10 Math/floor inc)
    (reduce #(if (> (count %2) %1) (count %2) %1)
            Double/NEGATIVE_INFINITY array)))

(defn array-of-numbers? [array]
  (number? (first array)))

(defn create-buckets [num-buckets]
  (repeat num-buckets []))
