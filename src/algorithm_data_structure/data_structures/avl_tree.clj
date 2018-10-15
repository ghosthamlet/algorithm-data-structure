(ns algorithm-data-structure.data-structures.avl-tree
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/tree/avl-tree"
  (:require [algorithm-data-structure.data-structures.binary-search-tree :as bst]
            [algorithm-data-structure.data-structures.binary-tree-node :as btn]))

(declare balance rotate-left-left rotate-left-right
         rotate-right-right rotate-right-left)

(defn create []
  (bst/create))

(require '[clojure.pprint :as pp])
(defn insert [self value]
  (loop [self (bst/insert self value)
         [current-node node-path] (bst/find* self value)]
    (if (empty? node-path)
      (balance self current-node node-path)
      (recur (balance self current-node node-path)
             [(bst/parent self node-path) (drop-last node-path)]))))

(defn remove* [self value]
  (throw (Exception. "Remove method is not implemented yet")))

(defn balance
  "When balance parent node, childs node must already balanced"
  [self node node-path]
  (cond
    (> (btn/balance-factor node) 1)
    (cond
      (pos? (btn/balance-factor (:left node)))
      (rotate-left-left self node node-path)
      (neg? (btn/balance-factor (:left node)))
      (rotate-left-right self node node-path)
      :else self)
    (< (btn/balance-factor node) -1)
    (cond
      (neg? (btn/balance-factor (:right node)))
      (rotate-right-right self node node-path)
      (pos? (btn/balance-factor (:right node)))
      (rotate-right-left self node node-path)
      :else self)
    :else self))

(defn rotate-left-left [self root-node node-path]
  (let [left-node (:left root-node)
        self (if (= root-node (:root self))
               (assoc self :root left-node)
               self)
        root-node (btn/set-left root-node nil)
        root-node (if (:right left-node)
                    (btn/set-left root-node (:right left-node))
                    root-node)
        parent-path (drop-last node-path)
        self (if (> (count node-path) 0)
               (update-in self (concat [:root] parent-path)
                          btn/set-left left-node)
               self)]
    (update-in self (concat [:root] node-path)
               btn/set-right root-node)))

(defn rotate-left-right [self root-node node-path]
  (let [left-node (:left root-node)
        root-node (btn/set-left root-node nil)
        left-right-node (:right left-node)
        left-node (btn/set-right left-node nil)
        [left-node left-right-node]
        (if (:left left-right-node)
          [(btn/set-right left-node (:left left-right-node))
           (btn/set-left left-right-node nil)]
          [left-node left-right-node])
        left-right-node (btn/set-left left-right-node left-node)
        root-node (btn/set-left root-node left-right-node)
        self (assoc-in self (concat [:root] node-path) root-node)
        self (assoc-in self (concat [:root] node-path [:left]) left-node)]
    (rotate-left-left self root-node node-path)))

(defn rotate-right-right [self root-node node-path]
  (let [right-node (:right root-node)
        self (if (= root-node (:root self))
               (assoc self :root right-node)
               self)
        root-node (btn/set-right root-node nil)
        root-node (if (:left right-node)
                    (btn/set-right root-node (:left right-node))
                    root-node)
        parent-path (drop-last node-path)
        self (if (> (count node-path) 0)
               (update-in self (concat [:root] parent-path)
                          btn/set-right right-node)
               self)]
    (update-in self (concat [:root] node-path)
               btn/set-left root-node)))

(defn rotate-right-left [self root-node node-path]
  (let [right-node (:right root-node)
        root-node (btn/set-right root-node nil)
        right-left-node (:left right-node)
        right-node (btn/set-left right-node nil)
        [right-node right-left-node]
        (if (:right right-left-node)
          [(btn/set-left right-node (:right right-left-node))
           (btn/set-right right-left-node nil)]
          [right-node right-left-node])
        right-left-node (btn/set-right right-left-node right-node)
        root-node (btn/set-right root-node right-left-node)
        self (assoc-in self (concat [:root] node-path) root-node)
        ;; self (assoc-in self (concat [:root] node-path [:right]) right-node)
        ]
    (rotate-right-right self root-node node-path)))


