(ns algorithm-data-structure.data-structures.graph-edge
  (:require [algorithm-data-structure.data-structures.graph-vertex :as gvertex]))

(defn create
  ([start-vertex end-vertex]
   (create start-vertex end-vertex 0))
  ([start-vertex end-vertex weight]
   {:start-vertex start-vertex
    :end-vertex end-vertex
    :weight weight}))

(defn get-key [gedge]
  (format "%s_%s"
          (gvertex/get-key (:start-vertex gedge))
          (gvertex/get-key (:end-vertex gedge))))

(defn reverse [gedge]
  (assoc gedge
         :start-vertex (:end-vertex gedge)
         :end-vertex (:start-vertex gedge)))

(defn ->string [gedge]
  (get-key gedge))
