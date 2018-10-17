(ns algorithm-data-structure.data-structures.disjoint-set-item
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/disjoint-set")

(defn create
  ([value] (create value nil))
  ([value key-callback]
   {:value value
    :key-callback key-callback
    :parent nil
    :children {}}))

(defn get-key [self]
  (if (:key-callback self)
    ((:key-callback self) (:value self))
    (:value self)))

(defn root? [self]
  (nil? (:parent self)))

(defn get-root [self]
  (if (root? self)
    self
    (get-root (:parent self))))

(defn get-children [self]
  (vals (:children self)))

(defn get-rank [self]
  (if (->> self get-children count zero?)
    0
    (reduce #(+ %1 1 (get-rank %2))
            0 (get-children self))))

(declare add-child)

(defn set-parent
  ([self parent-item]
   (set-parent self parent-item true))
  ([self parent-item force-setting-parent-child]
   (assoc self
          :parent (if force-setting-parent-child
                    (add-child parent-item self)
                    parent-item))))

(defn add-child [self child-item]
  (assoc-in self [:children (get-key child-item)]
            (set-parent child-item self false)))
