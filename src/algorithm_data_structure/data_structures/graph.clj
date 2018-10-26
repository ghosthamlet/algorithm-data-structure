(ns algorithm-data-structure.data-structures.graph
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/graph"
  (:refer-clojure :exclude [reverse])
  (:require [algorithm-data-structure.data-structures.graph-edge :as ge]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]))

(defn create
  ([] (create false))
  ([directed?]
   {:vertices {}
    :edges {}
    :directed? directed?}))

(defn add-vertex [self new-vertex]
  (update self
          :vertices
          assoc (gv/get-key new-vertex) new-vertex))

(defn get-vertex-by-key [self vertex-key]
  (get-in self [:vertices vertex-key]))

(defn assoc-vertex-by-key [self vertex-key v]
  (assoc-in self [:vertices vertex-key] v))

;; XXX: neighbor vertex on vertex edge may no edges data, can only use its key
(defn get-neighbors [self vertex]
  (gv/get-neighbors vertex))

(defn get-all-vertices [self]
  (vals (:vertices self)))

(defn get-all-edges [self]
  (vals (:edges self)))

(defn get-edge-vertex-key [edge start-or-end]
  (->> edge start-or-end gv/get-key))

(defn get-edge-vertex [self edge start-or-end]
  (->> (get-edge-vertex-key edge start-or-end)
       (get-vertex-by-key self)))

(defn add-edge [self edge]
  (let [sf (fn [g k v]
             (assoc-vertex-by-key g (get-edge-vertex-key edge k) v))
        vf (fn [g k]
             (if-let [vertex (get-edge-vertex g edge k)]
               [g vertex]
               (let [g (add-vertex g (k edge))]
                 [g (get-edge-vertex g edge k)])))
        [self start-vertex] (vf self :start-vertex)
        [self end-vertex] (vf self :end-vertex)
        self (if ((:edges self) (ge/get-key edge))
                 (throw (Exception. "Edge has already been added before"))
                 (update self :edges assoc (ge/get-key edge) edge))
        self (sf self :start-vertex (gv/add-edge start-vertex edge))]
    (if (:directed? self)
      self
      (sf self :end-vertex (gv/add-edge end-vertex edge)))))

(defn delete-edge [self edge]
  (if ((:edges self) (ge/get-key edge))
    (let [self (update self :edges dissoc (ge/get-key edge))
          sf (fn [g k]
               (assoc-vertex-by-key g
                                    (get-edge-vertex-key edge k)
                                    (gv/delete-edge (get-edge-vertex g edge k) edge)))
          self (sf self :start-vertex)]
      (sf self :end-vertex))
    (throw (Exception. "Edge not found in graph"))))

(defn find-edge [self start-vertex end-vertex]
  (gv/find-edge (get-vertex-by-key self
                                   (gv/get-key start-vertex))
                end-vertex))

(defn find-vertex-by-key [self vertex-key]
  ((:vertices self) vertex-key))

(defn get-weight [self]
  (reduce #(+ %1 (:weight %2)) 0 (get-all-edges self)))

(defn reverse [self]
  (reduce #(->> %2
                ge/reverse
                (add-edge (delete-edge %1 %2)))
          self
          (get-all-edges self)))

(defn get-vertices-indices [self]
  (last (reduce (fn [[i vs] v]
                  [(inc i)
                   (assoc vs (gv/get-key v) i)])
                [0 {}]
                (get-all-vertices self))))

(defn get-adjacency-matrix [self]
  (let [vertices (get-all-vertices self)
        vertices-indices (get-vertices-indices self)
        adjacency-matrix (let [xs (range (count vertices))]
                           (mapv (fn [_]
                                  (mapv (fn [_] Integer/MAX_VALUE) xs))
                                xs))]
    (last (reduce (fn [[i vs] vertex]
                    [(inc i)
                     (reduce #(assoc-in %1
                                        [i (vertices-indices (gv/get-key %2))]
                                        (:weight (find-edge self vertex %2)))
                             vs (gv/get-neighbors vertex))])
                  [0 adjacency-matrix]
                  vertices))))

(defn ->string [self]
  (str (keys (:vertices self))))
