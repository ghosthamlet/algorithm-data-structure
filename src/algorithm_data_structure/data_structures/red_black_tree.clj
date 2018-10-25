(ns algorithm-data-structure.data-structures.red-black-tree
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/tree/red-black-tree"
  (:require [algorithm-data-structure.data-structures.binary-search-tree :as bst]
            [algorithm-data-structure.data-structures.binary-search-tree-node :as bstn]
            [algorithm-data-structure.data-structures.binary-tree-node :as btn]
            [algorithm-data-structure.data-structures.hash-table :as ht]
            [algorithm-data-structure.comparator :refer :all]))

(declare balance node-black? node-red?
         left-left-rotation left-right-rotation right-right-rotation
         right-left-rotation make-node-black make-node-red
         swap-node-colors)

(def red-black-tree-colors
  {:red "red"
   :black "black"})

(def color-prop-name "color")

(defn create []
  (bst/create))

(defn insert [self value]
  (let [self (bst/insert self value)
        [inserted-node _] (bst/find* self value)
        self (if (equal self inserted-node (:root self))
              (make-node-black self inserted-node)
              (make-node-red self inserted-node))
        inserted-node (bst/find* self value)]
    (balance self inserted-node)))

(defn remove* [self value]
  (throw (Exception. (format "Can't remove %s. Remove method is not implemented yet" value))))

(defn balance [self node]
  (let [parent-node (bst/find-parent self node)]
    (cond
     (equal self node (:root self)) self
     (or (not parent-node) (node-black? self parent-node)) self
     :else (let [grand-parent-node (bst/find-parent self parent-node)
                 uncle (bst/uncle self node)]
             (cond (and uncle (node-red? self uncle))
                   ((if-not (equal self grand-parent-node (:root self))
                      #(balance (make-node-red % grand-parent-node) grand-parent-node)
                      identity)
                    (make-node-black (make-node-black self uncle) parent-node))
                   (or (not uncle) (node-black? self uncle) grand-parent-node)
                   ;; FIXME: new-grand-parent-node have to update swaped color in rotation
                   (let [[self new-grand-parent-node] (if (equal self (:left grand-parent-node) parent-node)
                                                        (if (equal self (:left parent-node) node)
                                                          (left-left-rotation self grand-parent-node)
                                                          (left-right-rotation self grand-parent-node))
                                                        (if (equal self (:right parent-node) node)
                                                          (right-right-rotation self grand-parent-node)
                                                          (right-left-rotation self grand-parent-node)))]
                     (balance (if (and new-grand-parent-node (not (:has-parent new-grand-parent-node)))
                                #_(make-node-black (assoc self :root new-grand-parent-node) new-grand-parent-node)
                                (make-node-black self (:root self))
                                self)
                              new-grand-parent-node))
                   :else self)))))

(defn left-left-rotation [self grand-parent-node]
  (let [grand-grand-parent-node (bst/find-parent self grand-parent-node)
        grand-parent-node-is-left (when grand-grand-parent-node
                                    (equal self (:left grand-grand-parent-node) grand-parent-node))
        parent-node (:left grand-parent-node)
        parent-right-node (:right parent-node)
        grand-parent-node (btn/set-left grand-parent-node parent-right-node)
        parent-node (btn/set-right parent-node grand-parent-node)
        [grand-grand-parent-node parent-node]
        (if grand-grand-parent-node
          [(if grand-parent-node-is-left
             (btn/set-left grand-grand-parent-node parent-node)
             (btn/set-right grand-grand-parent-node parent-node))
           parent-node]
          [grand-grand-parent-node (assoc parent-node :has-parent false)])
        self (if grand-grand-parent-node
                (bst/assoc* self grand-grand-parent-node)
                (assoc self :root parent-node))]
    ;; parent-node have to before grand-parent-node, or while swap, the child of parent-node will override grand-parent-node
    [(swap-node-colors self parent-node grand-parent-node)
     ;; this parent-node did not swap color
     parent-node]))

(defn left-right-rotation [self grand-parent-node]
  (let [parent-node (:left grand-parent-node)
        child-node (:right parent-node)
        child-left-node (:left child-node)
        parent-node (btn/set-right parent-node child-left-node)
        child-node (btn/set-left child-node parent-node)
        grand-parent-node (btn/set-left grand-parent-node child-node)
        self (-> self
                (bst/assoc* grand-parent-node))]
    (left-left-rotation self grand-parent-node)))

(defn right-right-rotation [self grand-parent-node]
  (let [grand-grand-parent-node (bst/find-parent self grand-parent-node)
        grand-parent-node-is-left (when grand-grand-parent-node
                                    (equal self (:left grand-grand-parent-node) grand-parent-node))
        parent-node (:right grand-parent-node)
        parent-left-node (:left parent-node)
        grand-parent-node (btn/set-right grand-parent-node parent-left-node)
        parent-node (btn/set-left parent-node grand-parent-node)
        [grand-grand-parent-node parent-node]
        (if grand-grand-parent-node
          [(if grand-parent-node-is-left
             (btn/set-left grand-grand-parent-node parent-node)
             (btn/set-right grand-grand-parent-node parent-node))
           parent-node]
          [grand-grand-parent-node (assoc parent-node :has-parent false)])
        self (if grand-grand-parent-node
                (bst/assoc* self grand-grand-parent-node)
                (assoc self :root parent-node))]
    [(swap-node-colors self parent-node grand-parent-node)
     parent-node]))

(defn right-left-rotation [self grand-parent-node]
  (let [parent-node (:right grand-parent-node)
        child-node (:left parent-node)
        child-right-node (:right child-node)
        parent-node (btn/set-left parent-node child-right-node)
        child-node (btn/set-right child-node parent-node)
        grand-parent-node (btn/set-right grand-parent-node child-node)
        self (-> self
                (bst/assoc* grand-parent-node))]
    (right-right-rotation self grand-parent-node)))

(defn- make-node-color [self node color]
  (bst/assoc* self
              (update node :meta ht/set* color-prop-name color)))

(defn make-node-red [self node]
  (make-node-color self node (:red red-black-tree-colors)))

(defn make-node-black [self node]
  (make-node-color self node (:black red-black-tree-colors)))

(defn- node-color? [self node color]
  (= (ht/get* (:meta node) color-prop-name)
     color))

(defn node-red? [self node]
  (node-color? self node (:red red-black-tree-colors)))

(defn node-black? [self node]
  (node-color? self node (:black red-black-tree-colors)))

(defn node-colored? [self node]
  (or (node-red? self node) (node-black? self node)))

(defn swap-node-colors [self first-node second-node]
  (let [first-color (ht/get* (:meta first-node) color-prop-name)
        second-color (ht/get* (:meta second-node) color-prop-name)]
    (-> self
        (make-node-color first-node second-color)
        (make-node-color second-node first-color))))
