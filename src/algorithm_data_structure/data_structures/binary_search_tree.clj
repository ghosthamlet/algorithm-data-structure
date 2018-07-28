(ns algorithm-data-structure.data-structures.binary-search-tree
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

(defn contains* [bst value]
  (bstn/contains* (:root bst) value))

(defn remove* [bst value]
  (bstn/remove* (:root bst) value))

(defn ->string [bst]
  (bstn/->string (:root bst)))
