(ns algorithm-data-structure.data-structures.binary-search-tree
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/tree/binary-search-tree"
  (:require [algorithm-data-structure.data-structures.binary-search-tree-node :as bstn]
            [algorithm-data-structure.comparator :refer :all]))

(defn create []
  (let [root (bstn/create nil)]
    {:root root}))

(defn parent [self child-path]
  (get-in self (cons :root (drop-last child-path))))

(defn insert [self value]
  {:root (bstn/insert (:root self) value)})

(defn find* [self value]
  (bstn/find* (:root self) value))

(defn find-path [self node]
  (last (find* self (:value node))))

(defn find-parent [self node]
  (->> [:value] (get-in node) (find* self) last (parent self)))

(defn uncle [self node]
  (when-let [parent-node (find-parent self node)]
    (when-let [parent-node2 (find-parent self parent-node)]
      (when-not (or (not (:left parent-node2)) (not (:right parent-node2)))
        (if (equal self parent-node (:left parent-node2))
          (:right parent-node2)
          (:left parent-node2))))))

;; (defn assoc* [bst node]
;;  (assoc-in self (concat [:root] (find-path self node)) node))

(defn contains?* [self value]
  (bstn/contains?* (:root self) value))

(defn remove* [self value]
  (let [[root res] (bstn/remove* (:root self) value)]
    [{:root root} res]))

(defn ->string [self]
  (bstn/->string (:root self)))
