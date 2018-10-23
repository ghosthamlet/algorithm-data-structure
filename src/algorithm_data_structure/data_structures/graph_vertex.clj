(ns algorithm-data-structure.data-structures.graph-vertex
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/graph"
  (:require [algorithm-data-structure.data-structures.linked-list :as ll]
            [algorithm-data-structure.comparator :refer :all])
  (:import [algorithm_data_structure.data_structures.linked_list LinkedList]))

(declare get-key)

(defrecord GraphVertex [value edges])

(defn create [value]
  (GraphVertex. value (ll/create)))

(defmethod compare-value [LinkedList GraphVertex] [_ a b]
  (compare-default nil (get-key a) (get-key b)))

(defn add-edge [self edge]
  (update self :edges ll/append edge))

(defn delete-edge [self edge]
  (update self :edges #(first (ll/delete % edge))))

(defn get-neighbors [self]
  (map #(let [v (get-in % [:value :start-vertex])]
          ;; FIXME: vertex self and v have to be same object, not just eq value
          (if (= (get-key self) (get-key v))
            (get-in % [:value :end-vertex])
            v))
       (ll/->array (:edges self))))

(defn get-edges [self]
  (map :value (ll/->array (:edges self))))

(defn get-degree [self]
  (count (ll/->array (:edges self))))

(defn has-edge [self required-edge]
  (boolean (ll/find* (:edges self)
                     nil
                     #(= % required-edge))))

(defn has-neighbor [self vertex]
  (boolean (ll/find* (:edges self)
                     nil
                     #(or (= (:start-vertex %) vertex)
                          (= (:end-vertex %) vertex)))))

(defn find-edge [self vertex]
  (when-let [edge (ll/find* (:edges self)
                            nil
                            #(or (= (:start-vertex %) vertex)
                                 (= (:end-vertex %) vertex)))]
    (:value edge)))

(defn get-key [self]
  (:value self))

(defn delete-all-edges [self]
  (reduce delete-edge self (get-edges self)))

(defn ->string [self callback]
  (if callback
    (callback (:value self))
    (str (:value self))))
