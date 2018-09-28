(ns algorithm-data-structure.data-structures.hash-table
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/hash-table"
  (:require [algorithm-data-structure.data-structures.linked-list :as ll]
            [algorithm-data-structure.comparator :refer :all]
            [algorithm-data-structure.util :refer :all])
  (:import [algorithm_data_structure.data_structures.linked_list LinkedList]))

(def default-hash-table-size 32)

;; HashTable may contain value with diff type, and compare just used equal
(defmethod compare-value LinkedList [self a b]
  (if (= (:value a) (:value b)) 0 -1))

(defn create
  ([] (create default-hash-table-size))
  ([hash-table-size]
   {:keys {}
    :buckets (vec (map (fn [_] (ll/create))
                       (range hash-table-size)))}))

(defn hash [self key]
  (let [hash (reduce #(+ %1 (int %2)) 0 key)]
    (->> self :buckets count (mod hash))))

(defn set* [self key value]
  (let [key-hash (hash self key)
        self (assoc-in self [:keys key] key-hash)
        bucket-linked-list (get-in self [:buckets key-hash])
        node (ll/find* bucket-linked-list
                          nil #(= (:key %) key))]
    (update-in (if-not node
                 self
                 (let [[ll _] (ll/delete (get-in self [:buckets key-hash])
                                         (:value node))]
                   (assoc-in self [:buckets key-hash] ll)))
               [:buckets key-hash]
               ll/append (->m key value))))

(defn delete [self key]
  (let [key-hash (hash self key)
        self (update self :keys dissoc key)
        bucket-linked-list (get-in self [:buckets key-hash])
        node (ll/find* bucket-linked-list
                       nil #(= (:key %) key))]
    (if node
      (let [[ll deleted] (ll/delete (get-in self [:buckets key-hash])
                                    (:value node))]
        [(assoc-in self [:buckets key-hash] ll) deleted])
      [self nil])))

(defn get* [self key]
  (let [bucket-linked-list (get-in self [:buckets (hash self key)])
        node (ll/find* bucket-linked-list
                          nil #(= (:key %) key))]
    (when node
      (get-in node [:value :value]))))

(defn has [self key]
  (contains? (:keys self) key))

(defn get-keys [self]
  (keys (:keys self)))
