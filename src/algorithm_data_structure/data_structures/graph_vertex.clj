(ns algorithm-data-structure.data-structures.graph-vertex
  (:require [algorithm-data-structure.data-structures.linked-list :as llist]))

(declare get-key)

(defn compare-edge
  [a b]
  (if (= (get-key a) (get-key b))
    0
    (if (< (get-key a) (get-key b)) -1 1)))

(defn create [value]
  {:value value
   :edges (llist/create)})

(defn add-edge [gvertex edge]
  (update gvertex :edges llist/append edge))

(defn delete-edge [gvertex edge]
  (update gvertex :edges llist/delete edge compare-edge))

(defn get-neighbors [gvertex]
  (map #(let [v (get-in % [:value :start-vertex])]
          (if (= gvertex v)
            (get-in % [:value :end-vertex])
            v))
       (llist/->array (:edges gvertex))))

(defn get-edges [gvertex]
  (map :value (llist/->array (:edges gvertex))))

(defn get-degree [gvertex]
  (count (llist/->array (:edges gvertex))))

(defn has-edge [gvertex required-edge]
  (boolean (llist/find* (:edges gvertex)
                        nil
                        #(= % required-edge)
                        compare-edge)))

(defn has-neighbor [gvertex vertex]
  (boolean (llist/find* (:edges gvertex)
                        nil
                        #(or (= (:start-vertex %) vertex)
                             (= (:end-vertex %) vertex))
                        compare-edge)))

(defn find-edge [gvertex vertex]
  (when-let [edge (llist/find* (:edges gvertex)
                               nil
                               #(or (= (:start-vertex %) vertex)
                                    (= (:end-vertex %) vertex)))]
    (:value edge)))

(defn get-key [gvertex]
  (:value gvertex))

(defn delete-all-edges [gvertex]
  (reduce delete-edge gvertex (get-edges gvertex)))

(defn ->string [gvertex callback]
  (if callback
    (callback (:value gvertex))
    (str (:value gvertex))))
