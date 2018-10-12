(ns algorithm-data-structure.data-structures.binary-tree-node
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/tree
  https://eddmann.com/posts/binary-search-trees-in-clojure/"
  (:require [algorithm-data-structure.data-structures.hash-table :as ht]
            [algorithm-data-structure.comparator :refer :all]
            [algorithm-data-structure.util :refer :all]))

(defn create
  ([] (create nil))
  ([value]
   {:left nil
    :right nil
    ;; clojure can't reference self
    ;; :parent
    :has-parent true
    :value value
    :meta (ht/create)}))

(defn left-height [self]
  (if-not (:left self)
    0
    (inc (left-height (:left self)))))

(defn right-height [self]
  (if-not (:right self)
    0
    (inc (right-height (:right self)))))

(defn height [self]
  (Math/max (left-height self) (right-height self)))

(defn balance-factor [self]
  (- (left-height self) (right-height self)))

(defn uncle [self]
  (throw (Exception. "Not implemented, clojure can't reference self, so can't get self's parent and relate data, use bst/uncle")))

(defn set-value [self value]
  (assoc self :value value))

(defn set-left [self node]
  (assoc self :left node))

(defn set-right [self node]
  (assoc self :right node))

(defn remove-child [self node-to-remove]
  (cond
    (and (:left self) (equal self (:left self) node-to-remove))
    [(assoc self :left nil) true]
    (and (:right self) (equal self (:right self) node-to-remove))
    [(assoc self :right nil) true]
    :else [self false]))

(defn replace-child [self node-to-replace replacement-node]
  (cond
    (or (not node-to-replace) (not replacement-node)) [self false]
    (and (:left self) (equal self (:left self) node-to-replace))
    [(assoc self :left replacement-node) true]
    (and (:right self) (equal self (:right self) node-to-replace))
    [(assoc self :right replacement-node) true]
    :else [self false]))

(defn copy-node [source-node target-node]
  source-node
  #_(-> target-node
        (set-value (:value source-node))
        (set-left (:left source-node))
        (set-right (:right source-node))))

(defn traverse-in-order [self]
  (concat []
          (when (:left self)
            (traverse-in-order (:left self)))
          [(:value self)]
          (when (:right self)
            (traverse-in-order (:right self)))))

(defn ->string [self]
  (str (vec (traverse-in-order self))))
