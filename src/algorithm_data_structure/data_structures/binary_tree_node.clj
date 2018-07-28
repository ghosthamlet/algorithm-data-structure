(ns algorithm-data-structure.data-structures.binary-tree-node
  (:require [algorithm-data-structure.data-structures.hash-table :as ht]
            [algorithm-data-structure.util :refer :all]))

;; https://eddmann.com/posts/binary-search-trees-in-clojure/
(defn create
  ([] (create nil))
  ([value]
   ;; (update-in xs (recurive-fn xs) data)
   {:left nil
    :right nil
    ;; :parent purely functional data can't reference self
    :value value
    :meta (ht/create)
    :node-comparator comparator-fns}))

(defn- compare* [bstn type]
  (get-in bstn [:node-comparator type]))

(defn- equal [bstn a b]
  ((compare* bstn :equal) a b))

(defn- less-then [bstn a b]
  ((compare* bstn :less-then) a b))

(defn- greater-then [bstn a b]
  ((compare* bstn :greater-then) a b))


(defn left-height [btn]
  (if-not (:left btn)
    0
    (inc (get-in btn [:left :height]))))

(defn right-height [btn]
  (if-not (:right btn)
    0
    (inc (get-in btn [:right :height]))))

(defn height [btn]
  (Math/max (left-height btn) (right-height btn)))

(defn balance-factor [btn]
  (- (right-height btn) (left-height btn)))

(defn uncle [btn]
  (throw (Exception. "Not implemented, purely functional data can't reference self, so can't get self's parent and relate data")))

(defn set-value [btn value]
  (assoc btn :value value))

(defn set-left [btn node]
  (assoc btn :left node))

(defn set-right [btn node]
  (assoc btn :right node))

(defn remove-child [btn node-to-remove]
  (cond
    (and (:left btn) (equal btn (:left btn) node-to-remove))
    [(assoc btn :left nil) true]
    (and (:right btn) (equal btn (:right btn) node-to-remove))
    [(assoc btn :right nil) true]
    :else [btn false]))

(defn replace-child [btn node-to-replace replacement-node]
  (cond
    (or (not node-to-replace) (not replacement-node)) [btn false]
    (and (:left btn) (equal btn (:left btn) node-to-replace))
    [(assoc btn :left replacement-node) true]
    (and (:right btn) (equal btn (:right btn) node-to-replace))
    [(assoc btn :right replacement-node) true]
    :else [btn false]))

(defn copy-node [source-node target-node]
  (-> target-node
      (set-value (:value source-node))
      (set-left (:left source-node))
      (set-right (:right source-node))))

(defn traverse-in-order [btn]
  (concat []
          (when (:left btn)
            (traverse-in-order (:left btn)))
          [(:value btn)]
          (when (:right btn)
            (traverse-in-order (:right btn)))))

(defn ->string [btn]
  (str (vec (traverse-in-order btn))))
