(ns algorithm-data-structure.algorithms.sets.knapsack-item)

(defn create [{:keys [value weight items-in-stock]
               :or {items-in-stock 1}}]
  {:value value
   :weight weight
   :items-in-stock items-in-stock
   :quantity 1})

(defn get-total-value [ki]
  (* (:value ki) (:quantity ki)))

(defn get-total-weight [ki]
  (* (:weight ki) (:quantity ki)))

(defn get-value-per-weight-ratio [ki]
  (/ (:value ki) (:weight ki)))

(defn ->string [ki]
  (format "v%s %s x %s" (:value ki) (:weight ki) (:quantity ki)))
