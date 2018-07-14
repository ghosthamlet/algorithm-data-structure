(ns algorithm-data-structure.data-structures.hash-table
  (:require [algorithm-data-structure.data-structures.linked-list :as llist]
            [algorithm-data-structure.util :refer :all]))

(def default-hash-table-size 32)

(defn create
  ([] (create default-hash-table-size))
  ([hash-table-size]
   {:keys {}
    :buckets (vec (map #(llist/create)
                       (range hash-table-size)))}))

(defn hash [ht key]
  (let [hash (reduce #(+ %1 (int %2)) 0 key)]
    (->> ht :buckets count (mod hash))))

(defn set [ht key value]
  (let [key-hash (hash ht key)
        ht (assoc-in ht [:keys key] key-hash)
        bucket-linked-list (get-in ht [:buckets key-hash])
        node (llist/find* bucket-linked-list
                          nil #(= (:key %) key))]
    (update-in (if (not node)
                 ht
                 (update-in ht [:buckets key-hash]
                            llist/delete (:value node)))
               [:buckets key-hash]
               llist/append (->m key value))))

(defn delete [ht key]
  (let [key-hash (hash ht key)
        ht (update ht :keys dissoc key)
        bucket-linked-list (get-in ht [:buckets key-hash])
        node (llist/find* bucket-linked-list
                          nil #(= (:key %) key))]
    (when node
      (update-in ht [:buckets key-hash]
                 llist/delete (:value node)))))

(defn get* [ht key]
  (let [bucket-linked-list (get-in ht [:buckets (hash ht key)])
        node (llist/find* bucket-linked-list
                          nil #(= (:key %) key))]
    (when node
      (get-in node [:value :value]))))

(defn has [ht key]
  (contains? (:keys ht) key))

(defn get-keys [ht]
  (keys (:keys ht)))
