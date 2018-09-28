(ns algorithm-data-structure.data-structures.binary-search-tree
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/tree/binary-search-tree"
  (:require [algorithm-data-structure.data-structures.binary-search-tree-node :as bstn]))

(defn create [node-value-compare-function]
  (let [root (bstn/create nil node-value-compare-function)]
    {:root root
     :node-comparator (:node-comparator root)}))

(defn parent [bst child-path]
  (get-in bst [:root (drop-last child-path)]))

(defn insert [bst value]
  (bstn/insert (:root bst) value))

(defn find* [bst value]
  (bstn/find* (:root bst) value))

(defn find-path [bst node]
  (last (find* bst (:value node))))

(defn find-parent [bst node]
  (->> [:value] (get-in node) (find* bst) last (parent bst)))

(defn uncle [bst node]
  (when-let [parent-node (find-parent bst node)]
    (when-let [parent-node2 (find-parent bst parent-node)]
      (when-not (or (not (:left parent-node2)) (not (:right parent-node2)))
        (if (equal bst parent-node (:left parent-node2))
          (:right parent-node2)
          (:left parent-node2))))))

(defn assoc* [bst node]
  (assoc-in bst (concat [:root] (find-path bst node)) node))

(defn contains* [bst value]
  (bstn/contains* (:root bst) value))

(defn remove* [bst value]
  (bstn/remove* (:root bst) value))

(defn ->string [bst]
  (bstn/->string (:root bst)))
