(ns algorithm-data-structure.data-structures.binary-search-tree-node
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/tree/binary-search-tree"
  (:require [algorithm-data-structure.data-structures.binary-tree-node :as btn]
            [algorithm-data-structure.comparator :refer :all]))

(declare find-min)

(defn create
  ([] (create nil))
  ([value]
   (btn/create value)))

(defn insert [self value]
  (let [current-value (:value self)
        f #(if (%1 self)
             (%2 self (insert (%1 self) value))
             (let [new-node (create value)]
               (%2 self new-node)))]
    (cond
      (equal self current-value nil) (assoc self :value value)
      (less-then self value current-value) (f :left btn/set-left)
      (greater-than self value current-value) (f :right btn/set-right))))

(defn find* [self value]
  (loop [self self
         node-path []]
    (let [current-value (:value self)]
     (cond
       (equal self value current-value)
       [self node-path]
       (and (less-then self value current-value) (:left self))
       (recur (:left self) (conj node-path :left))
       (and (greater-than self value current-value) (:right self))
       (recur (:right self) (conj node-path :right))
       :else [nil []]))))

(defn contains?* [self value]
  (-> self (find* value) first boolean))

(defn- set-child-value [self value node-path]
  (update-in self node-path btn/set-value value))

(defn- set-child-right [self value node-path]
  (update-in self node-path btn/set-right value))

(defn- remove-child [self node-to-remove remove-path]
  (if (> (count remove-path) 1)
    (update-in self (drop-last remove-path)
               #(first (btn/remove-child % node-to-remove)))
    (first (btn/remove-child self node-to-remove))))

(defn- replace-child [self node-to-remove child-node remove-path]
  (if (> (count remove-path) 1)
    (update-in self (drop-last remove-path)
               #(first (btn/replace-child % node-to-remove child-node)))
    (first (btn/replace-child self node-to-remove child-node))))

(defn remove* [self value]
  (let [[node-to-remove remove-path] (find* self value)
        _ (when-not node-to-remove
            (throw (Exception. "Item not found in the tree")))
        has-parent (> (count remove-path) 0)
        right-child (:right node-to-remove)
        left-child (:left node-to-remove)]
    (cond
      (and (not left-child) (not right-child))
      (if has-parent
        [(remove-child self node-to-remove remove-path) true]
        [(btn/set-value node-to-remove nil) true])
      (and left-child right-child)
      (let [next-bigger-node (find-min right-child)
            bigger-value (:value next-bigger-node)]
        (if-not (equal self next-bigger-node right-child)
          [(-> self (remove* bigger-value) first
               (set-child-value bigger-value remove-path))
           true]
          [(-> self (set-child-value (:value right-child) remove-path)
               (set-child-right (:right right-child) remove-path))
           true]))
      :else
      (let [child-node (or left-child right-child)]
        (if has-parent
          [(replace-child self node-to-remove child-node remove-path) true]
          [(btn/copy-node child-node node-to-remove) true])))))

(defn find-min [self]
  (if-not (:left self)
    self
    (recur (:left self))))

(defn ->string [self]
  (btn/->string self))
