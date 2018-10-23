(ns algorithm-data-structure.data-structures.graph-edge
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/graph"
  (:refer-clojure :exclude [reverse])
  (:require [algorithm-data-structure.data-structures.graph-vertex :as gv]))

(defn create
  ([start-vertex end-vertex]
   (create start-vertex end-vertex 0))
  ([start-vertex end-vertex weight]
   {:start-vertex start-vertex
    :end-vertex end-vertex
    :weight weight}))

(defn get-key [self]
  (format "%s_%s"
          (gv/get-key (:start-vertex self))
          (gv/get-key (:end-vertex self))))

(defn reverse [self]
  (assoc self
         :start-vertex (:end-vertex self)
         :end-vertex (:start-vertex self)))

(defn ->string [self]
  (get-key self))
