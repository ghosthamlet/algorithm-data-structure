(ns algorithm-data-structure.data-structures.red-black-tree
  (:require [algorithm-data-structure.data-structures.binary-search-tree :as bst]
            [algorithm-data-structure.data-structures.binary-search-tree-node :as bstn]
            [algorithm-data-structure.data-structures.hash-table :as ht]))

(declare make-node-black make-node-red)

(def red-black-tree-colors
  {:red "red"
   :black "black"})

(def color-prop-name "color")

(defn create [node-value-compare-function]
  (bst/create node-value-compare-function))

(defn- compare* [bstn type]
  (get-in bstn [:node-comparator type]))

(defn- equal [bstn a b]
  ((compare* bstn :equal) a b))

(defn- less-then [bstn a b]
  ((compare* bstn :less-then) a b))

(defn- greater-then [bstn a b]
  ((compare* bstn :greater-then) a b))

(defn insert [rbt value]
  (let [inserted-node (bst/insert rbt value)
        rbt (if (equal rbt inserted-node (:root rbt))
              (make-node-black rbt inserted-node)
              (make-node-red rbt inserted-node))]
    (balance rbt inserted-node)))

(defn remove* [rbt value]
  (throw (Exception. (format "Can't remove %s. Remove method is not implemented yet" value))))

(defn balance [rbt node]
  (cond
    (equal rbt node (:root rbt)) rbt
    (node-black? rbt (bst/find-parent rbt node)) rbt
    :else (let [parent-node (bst/find-parent rbt node)
                grand-parent-node (bst/find-parent rbt parent-node)
                uncle (bst/uncle rbt node)]
            (cond (and uncle (node-red? uncle))
                  ((if-not (equal rbt grand-parent-node (:root rbt))
                     #(balance (make-node-red % grand-parent-node) grand-parent-node)
                     identity)
                   (make-node-black (make-node-black rbt uncle) parent-node))
                  (or (not uncle) (node-black? rbt uncle) grand-parent-node)
                  (let [[rbt new-grand-parent-node] (if (equal rbt (:left grand-parent-node) (parent-node))
                                                      (if (equal rbt (:left parent-node) node)
                                                        (left-left-rotation rbt grand-parent-node)
                                                        (left-right-rotation rbt grand-parent-node))
                                                      (if (equal rbt (:right parent-node) node)
                                                        (right-right-rotation rbt grand-parent-node)
                                                        (right-left-rotation rbt grand-parent-node)))]
                    (balance (if (and new-grand-parent-node
                                      (not (:has-parent new-grand-parent-node)))
                               (make-node-black (assoc rbt :root new-grand-parent-node)
                                                new-grand-parent-node)
                               rbt)
                             new-grand-parent-node))
                  :else rbt))))

(defn left-left-rotation [rbt grand-parent-node]
  (let [grand-grand-parent-node (bst/find-parent rbt grand-parent-node)
        grand-parent-node-is-left (when grand-grand-parent-node
                                    (equal rbt (:left grand-grand-parent-node) grand-parent-node))
        parent-node (:left grand-parent-node)
        parent-right-node (:right parent-node)
        parent-node (bstn/set-right parent-node grand-parent-node)
        grand-parent-node (bstn/set-left grand-parent-node parent-right-node)
        [grand-grand-parent-node parent-node]
        (if grand-grand-parent-node
          [(if grand-parent-node-is-left
             (bstn/set-left grand-grand-parent-node parent-node)
             (bstn/set-right grand-grand-parent-node parent-node))
           parent-node]
          [grand-grand-parent-node (assoc parent-node :has-parent false)])
        ;; must assoc max depth -> min, or the min depth path may modified before max depth
        rbt (-> rbt
                (bst/assoc* grand-grand-parent-node)
                (bst/assoc* grand-parent-node)
                (bst/assoc* parent-node))]
    [(swap-node-colors bst parent-node grand-parent-node) parent-node]))

(defn left-right-rotation [rbt grand-parent-node]
  (let [parent-node (:left grand-parent-node)
        child-node (:right parent-node)
        child-left-node (:left child-node)
        child-node (bstn/set-left child-node parent-node)
        parent-node (bstn/set-right parent-node child-left-node)
        grand-parent-node (bstn/set-left grand-parent-node child-node)
        rbt (-> rbt
                (bst/assoc* grand-parent-node)
                (bst/assoc* parent-node)
                (bst/assoc* child-node))]
    (left-left-rotation rbt grand-parent-node)))

(defn right-right-rotation [rbt grand-parent-node]
  (let [grand-grand-parent-node (bst/find-parent rbt grand-parent-node)
        grand-parent-node-is-left (when grand-grand-parent-node
                                    (equal rbt (:left grand-grand-parent-node) grand-parent-node))
        parent-node (:right grand-parent-node)
        parent-left-node (:left parent-node)
        parent-node (bstn/set-left parent-node grand-parent-node)
        grand-parent-node (bstn/set-right grand-parent-node parent-left-node)
        [grand-grand-parent-node parent-node]
        (if grand-grand-parent-node
          [(if grand-parent-node-is-left
             (bstn/set-left grand-grand-parent-node parent-node)
             (bstn/set-right grand-grand-parent-node parent-node))
           parent-node]
          [grand-grand-parent-node (assoc parent-node :has-parent false)])
        ;; must assoc max depth -> min, or the min depth path may modified before max depth
        rbt (-> rbt
                (bst/assoc* grand-grand-parent-node)
                (bst/assoc* grand-parent-node)
                (bst/assoc* parent-node))]
    [(swap-node-colors bst parent-node grand-parent-node) parent-node]))

(defn right-left-rotation [rbt grand-parent-node]
  (let [parent-node (:right grand-parent-node)
        child-node (:left parent-node)
        child-right-node (:right child-node)
        child-node (bstn/set-right child-node parent-node)
        parent-node (bstn/set-left parent-node child-right-node)
        grand-parent-node (bstn/set-right grand-parent-node child-node)
        rbt (-> rbt
                (bst/assoc* grand-parent-node)
                (bst/assoc* parent-node)
                (bst/assoc* child-node))]
    (right-right-rotation rbt grand-parent-node)))

(defn- make-node-color [rbt node color]
  (bst/assoc* rbt
              (update node :meta ht/set* color-prop-name color)))

(defn make-node-red [rbt node]
  (make-node-color rbt node (:red red-black-tree-colors)))

(defn make-node-black [rbt node]
  (make-node-color rbt node (:black red-black-tree-colors)))

(defn- node-color? [rbt node color]
  (= (ht/get* (:meta node) color-prop-name)
     color))

(defn node-red? [rbt node]
  (node-color? rbt node (:red red-black-tree-colors)))

(defn node-black? [rbt node]
  (node-color? rbt node (:black red-black-tree-colors)))

(defn node-colored? [rbt node]
  (or (node-red? rbt node) (node-black? rbt node)))

(defn swap-node-colors [rbt first-node second-node]
  (let [first-color (ht/get* (:meta first-node) color-prop-name)
        second-color (ht/get* (:meta second-node) color-prop-name)]
    (-> rbt
        (make-node-color first-node second-color)
        (make-node-color second-node first-color))))
