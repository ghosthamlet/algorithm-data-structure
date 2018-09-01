(ns algorithm-data-structure.algorithms.sets.knapsack
  (:require [algorithm-data-structure.algorithms.sorting.merge-sort :as ms]))

(defn create [possible-items weight-limit]
  {:selected-items []
   :weight-limit weight-limit
   :possible-items possible-items})

(defn sort-possible-items-by-weight [k]
  (update k
          :possible-items ms/run
          :comparator
          (fn [item-a item-b]
            (condp #(%1 (:weight item-a) %2)
                (:weight item-b)
              = 0
              < -1
              1))))

(defn sort-possible-items-by-value [k]
  (update k
          :possible-items ms/run
          :comparator
          (fn [item-a item-b]
            (condp #(%1 (:value item-a) %2)
                (:value item-b)
              = 0
              > -1
              1))))

(defn sort-possible-items-by-value-per-weight-ratio [k]
  (update k
          :possible-items ms/run
          :comparator
          (fn [item-a item-b]
            (condp #(%1 (:value-per-weight-ratio item-a) %2)
                (:value-per-weight-ratio item-b)
              = 0
              > -1
              1))))

(defn solve-zero-one-knapsack-problem [k]
  (let [k ((comp sort-possible-items-by-weight
                 sort-possible-items-by-value) k)
        k (assoc k :selected-items [])
        number-of-rows (count (:possible-items k))
        number-of-columns (:weight-limit k)
        knapsack-matrix (vec (repeat number-of-rows
                                     (vec (repeat (inc number-of-columns) nil))))
        knapsack-matrix (map #(assoc % 0 0) knapsack-matrix)
        knapsack-matrix (update knapsack-matrix
                                0 (fn [xs]
                                    (map-indexed #(if (zero? %1)
                                                    %2
                                                    (let [item-weight (get-in k [:possible-items 0 :weight])
                                                          item-value (get-in k [:possible-items 0 :value])]
                                                      (if (<= item-weight %2) item-value 0)))
                                                 xs)))
        knapsack-matrix (map-indexed (fn [item-index row]
                                       (if (= 0 item-index)
                                         row
                                         (map-indexed (fn [weight-index col]
                                                        (if (= 0 col)
                                                          col
                                                          (let [current-item-weight (get-in k [:possible-items item-index :weight])
                                                                current-item-value (get-in k [:possible-items item-index :value])
                                                                v (get-in knapsack-matrix [(dec item-index) weight-index])]
                                                            (if (> current-item-weight weight-index)
                                                              v
                                                              (max (+ current-item-value
                                                                      (get-in knapsack-matrix
                                                                              [(dec item-index) (- weight-index current-item-weight)]))
                                                                   v)))))
                                                      row)))
                                     knapsack-matrix)]
    (loop [item-index (dec number-of-rows)
           selected-items (:selected-items k)
           weight-index number-of-columns]
      (if (zero? item-index)
        (assoc k :selected-items selected-items)
        (let [current-item (get-in k [:possible-items item-index])
              prev-item (get-in k [:possible-items (dec item-index)])
              v (get-in knapsack-matrix [item-index weight-index])
              v2 (get-in knapsack-matrix [(dec item-index) weight-index])
              [selected-items weight-index]
              (cond
                (and v (= v v2))
                (let [prev-sum-value v2
                      prev-prev-sum-value (get-in knapsack-matrix [(- item-index 2) weight-index])]
                  (if (or (not prev-sum-value)
                          (and prev-sum-value (not= prev-prev-sum-value prev-sum-value)))
                    [(conj selected-items prev-item) weight-index]
                    [selected-items weight-index]))
                (get-in knapsack-matrix [(dec item-index) (- weight-index (:weight current-item))])
                [(conj selected-items prev-item) (- weight-index (:weight current-item))]
                :else [selected-items weight-index])]
          (recur (dec item-index)
                 selected-items
                 weight-index))))))


(defn solve-unbounded-knapsack-problem [k]
  (let [k ((comp sort-possible-items-by-value-per-weight-ratio
                 sort-possible-items-by-value) k)]
    (reduce (fn [acc current-item]
              (if (< (get-total-weight k) (:weight-limit k))
                (let [available-weight (- (:weight-limit k) (get-total-weight k))
                      max-possible-items-count (Math/floor (/ available-weight (:weight current-item)))]
                  (update acc
                          :selected-items
                          conj (cond
                                 (> max-possible-items-count (:items-in-stock current-item))
                                 (assoc current-item :quantity (:items-in-stock current-item))
                                 max-possible-items-count
                                 (assoc current-item :quantity max-possible-items-count)
                                 :else current-item)))
                acc))
            k (:possible-items k))))

(defn get-total-value [k]
  (reduce #(+ %1 (get-total-value %2)) 0 (:selected-items k)))

(defn get-total-weight [k]
  (reduce #(+ %1 (get-total-weight %2)) 0 (:selected-items k)))
