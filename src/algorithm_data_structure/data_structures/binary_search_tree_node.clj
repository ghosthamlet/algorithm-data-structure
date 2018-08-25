(ns algorithm-data-structure.data-structures.binary-search-tree-node
  (:require [algorithm-data-structure.data-structures.binary-tree-node :as btn]))

(defn create
  ([] (create nil nil))
  ([value] (create value nil))
  ([value comparator-function]
   ;; XXX: merge parent btn data to child mimic inheritance
   ;;      after merge, parent fns on merged data must not reset
   ;;      or recreate the whole data or it will lost the child data
   (merge (btn/create value)
          {:compare-function comparator-function
           :node-value-comparator comparator-function})))

(defn- compare* [bstn type]
  (get-in bstn [:node-value-comparator type]))

(defn- equal [bstn a b]
  ((compare* bstn :equal) a b))

(defn- less-then [bstn a b]
  ((compare* bstn :less-then) a b))

(defn- greater-then [bstn a b]
  ((compare* bstn :greater-then) a b))

(defn insert [bstn value]
  (let [current-value (:value bstn)
        f #(if (%1 bstn)
             (insert (%1 bstn) value)
             (let [new-node (create value (:compare-function bstn))]
               (%2 bstn new-node)))]
    (cond
      (equal bstn current-value nil) (assoc bstn :value value)
      (less-then bstn value current-value) (f :left btn/set-left)
      (greater-then bstn value current-value) (f :right btn/set-right))))

(defn find* [bstn value]
  (loop [bstn bstn
         node-path []]
    (let [current-value (:value bstn)]
     (cond
       (equal bstn value current-value)
       [bstn node-path]
       (and (less-then bstn value current-value) (:left bstn))
       (recur (:left bstn) (conj node-path :left))
       (and (greater-then bstn value current-value) (:right bstn))
       (recur (:right bstn) (conj node-path :right))
       :else [nil []]))))

(defn contains* [bstn value]
  (-> bstn (find* value) first boolean))

(defn- set-child-value [bstn value node-path]
  (update-in bstn node-path btn/set-value value))

(defn- set-child-right [bstn value node-path]
  (update-in bstn node-path btn/set-right value))

(defn- remove-child [bstn node-to-remove remove-path]
  (update-in bstn (drop-last remove-path)
             btn/remove-child node-to-remove))

(defn- replace-child [bstn node-to-remove child-node remove-path]
  (update-in bstn (drop-last remove-path)
             btn/replace-child node-to-remove child-node))

(defn remove* [bstn value]
  (let [[node-to-remove remove-path] (find* bstn value)
        _ (when-not node-to-remove
            (throw (Exception. "Item not found in the tree")))
        has-parent (> (count remove-path) 1)
        right-child (:right node-to-remove)
        left-child (:left node-to-remove)]
    (cond
      (and (not left-child) (not right-child))
      (if has-parent
        [(remove-child bstn node-to-remove remove-path) true]
        [(btn/set-value node-to-remove nil) true])
      (and left-child right-child)
      (let [next-bigger-node (find-min right-child)
            bigger-value (:value next-bigger-node)]
        (if-not (equal bstn next-bigger-node right-child)
          [(-> bstn (remove* bigger-value) first
               (set-child-value bigger-value remove-path))
           true]
          [(-> bstn (set-child-value (:value right-child) remove-path)
               (set-child-right (:right right-child) remove-path))
           true]))
      :else
      (let [child-node (or left-child right-child)]
        (if has-parent
          [(replace-child bstn node-to-remove child-node remove-path) true]
          [(copy-node child-node node-to-remove) true])))))

(defn find-min [bstn]
  (if-not (:left bstn)
    bstn
    (recur (:left bstn))))
