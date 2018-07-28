(ns algorithm-data-structure.data-structures.avl-tree
  (:require [algorithm-data-structure.data-structures.binary-search-tree :as bst]
            [algorithm-data-structure.data-structures.binary-search-tree-node :as bstn]))

(declare balance rotate-left-left rotate-left-right
         rotate-right-right rotate-right-left)

(defn create [node-value-compare-function]
  (bst/create node-value-compare-function))

(defn insert [at value]
  (loop [at (bst/insert at value)
         [current-node node-path] (bst/find* at value)]
    (if-not current-node
      at
      (recur (balance at current-node node-path)
             [(bst/parent at node-path) (drop-last node-path)]))))

(defn remove* [at value]
  (throw (Exception. "Remove method is not implemented yet")))

(defn balance [at node node-path]
  (cond
    (> (bstn/balance-factor node) 1)
    (cond
      (pos? (bstn/balance-factor (:left node)))
      (rotate-left-left at node node-path)
      (neg? (bstn/balance-factor (:left node)))
      (rotate-left-right at node node-path))
    (< (bstn/balance-factor node) -1)
    (cond
      (neg? (bstn/balance-factor (:right node)))
      (rotate-right-right at node node-path)
      (pos? (bstn/balance-factor (:right node)))
      (rotate-right-left at node node-path))))

(defn rotate-left-left [at root-node node-path]
  (let [left-node (:left root-node)
        root-node (bstn/set-left root-node nil)
        root-node (if (:right left-node)
                    (bstn/set-left root-node (:right left-node))
                    root-node)
        parent-path (drop-last node-path)
        at (if parent-path
             (update-in at (concat :root parent-path)
                        bstn/set-left left-node)
             at)
        at (if (= root-node (:root at))
             (assoc at :root left-node)
             at)]
    (update-in at (concat node-path :left)
               bstn/set-right root-node)))

(defn rotate-left-right [at root-node node-path]
  (let [left-node (:left root-node)
        root-node (bstn/set-left root-node nil)
        left-right-node (:right left-node)
        left-node (bstn/set-right left-node nil)
        [left-node left-right-node]
        (if (:left left-right-node)
          [(bstn/set-right left-node (:left left-right-node))
           (bstn/set-left left-right-node nil)]
          [left-node left-right-node])
        left-right-node (bstn/set-left left-right-node left-node)
        root-node (bstn/set-left root-node left-right-node)
        at (assoc-in at node-path root-node)
        at (assoc-in at (concat node-path :left) left-node)]
    (rotate-left-left at root-node node-path)))

(defn rotate-right-left [at root-node node-path]
  (let [right-node (:right root-node)
        root-node (bstn/set-right root-node nil)
        right-left-node (:left left-node)
        right-node (bstn/set-left right-node nil)
        [right-node right-left-node]
        (if (:right right-left-node)
          [(bstn/set-left right-node (:right right-left-node))
           (bstn/set-right right-left-node nil)]
          [right-node right-left-node])
        right-left-node (bstn/set-right right-left-node right-node)
        root-node (bstn/set-right root-node right-left-node)
        at (assoc-in at node-path root-node)
        at (assoc-in at (concat node-path :right) right-node)]
    (rotate-right-right at root-node node-path)))

(defn rotate-right-right [at root-node node-path]
  (let [right-node (:right root-node)
        root-node (bstn/set-right root-node nil)
        root-node (if (:left right-node)
                    (bstn/set-right root-node (:left right-node))
                    root-node)
        parent-path (drop-last node-path)
        at (if parent-path
             (update-in at (concat :root parent-path)
                        bstn/set-right right-node)
             at)
        at (if (= root-node (:root at))
             (assoc at :root right-node)
             at)]
    (update-in at (concat node-path :right)
               bstn/set-left root-node)))
