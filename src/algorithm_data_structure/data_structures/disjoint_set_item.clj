(ns algorithm-data-structure.data-structures.disjoint-set-item)

(defn create [value key-callback]
  {:value value
   :key-callback key-callback
   :parent nil
   :children {}})

(defn get-key [dsi]
  (if (:key-callback dsi)
    ((:key-callback dsi) (:value dsi))
    (:value dsi)))

(defn is-root [dsi]
  (nil? (:parent dsi)))

(defn get-root [dsi]
  (if (is-root dsi)
    dsi
    (get-root (:parent dsi))))

(defn get-children [dsi]
  (vals (:children dsi)))

(defn get-rank [dsi]
  (if (->> dsi get-children count zero?)
    0
    (reduce #(+ %1 1 (get-rank %2))
            0 (get-children dsi))))

(declare add-child)

(defn set-parent
  ([dsi parent-item]
   (set-parent dsi parent-item true))
  ([dsi parent-item force-setting-parent-child]
   (assoc dsi
          :parent (if force-setting-parent-child
                    (add-child parent-item dsi)
                    parent-item))))

(defn add-child [dsi child-item]
  (assoc-in dsi [:children (get-key child-item)]
            (set-parent child-item dsi false)))
