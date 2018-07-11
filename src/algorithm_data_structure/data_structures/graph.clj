(ns algorithm-data-structure.data-structures.graph
  (:refer-clojure :exclude [reverse])
  (:require [algorithm-data-structure.data-structures.graph-edge :as gedge]
            [algorithm-data-structure.data-structures.graph-vertex :as gvertex]))

(defn create
  ([] (create false))
  ([directed?]
   {:vertices {}
    :edges {}
    :directed? directed?}))

(defn add-vertex [ggraph new-vertex]
  (update ggraph
          :vertices
          assoc (gvertex/get-key new-vertex) new-vertex))

(defn get-vertex-by-key [ggraph vertex-key]
  (get-in ggraph [:vertices vertex-key]))

(defn assoc-vertex-by-key [ggraph vertex-key v]
  (assoc-in ggraph [:vertices vertex-key] v))

(defn get-neighbors [ggraph vertex]
  (gvertex/get-neighbors vertex))

(defn get-all-vertices [ggraph]
  (keys (:vertices ggraph)))

(defn get-all-edges [ggraph]
  (keys (:edges ggraph)))

(defn get-edge-key [edge k]
  (->> edge
       k
       gvertex/get-key))

(defn get-edge-vertex [ggraph edge k]
  (->> (get-edge-key edge k)
       (get-vertex-by-key ggraph)))

(defn add-edge [ggraph edge]
  (let [sf (fn [g k v]
             (assoc-vertex-by-key g
                                  (get-edge-key edge k)
                                  v))
        vertex-fn (fn [g k]
                    (if-let [vertex (get-edge-vertex g edge k)]
                      [g vertex]
                      (let [g (add-vertex g (k edge))]
                        [g (get-edge-vertex g edge k)])))
        [ggraph start-vertex] (vertex-fn ggraph :start-vertex)
        [ggraph end-vertex] (vertex-fn ggraph :end-vertex)
        ggraph (if ((gedge/get-key edge) (:edges ggraph))
                 (throw (Exception. "Edge has already been added before"))
                 (update ggraph :edges assoc (gedge/get-key edge) edge))
        ggraph (sf ggraph :start-vertex (gvertex/add-edge start-vertex edge))]
    (if (:directed? ggraph)
      ggraph
      (sf ggraph :end-vertex (gvertex/add-edge end-vertex edge)))))

(defn delete-edge [ggraph edge]
  (if ((gedge/get-key edge) (:edges ggraph))
    (let [ggraph (update ggraph :edges dissoc (gedge/get-key edge))
          sf (fn [g k]
               (assoc-vertex-by-key g
                                    (get-edge-key edge k)
                                    (gvertex/delete-edge (get-edge-vertex g edge k) edge)))
          ggraph (sf ggraph :start-vertex)]
      (sf ggraph :end-vertex))
    (throw (Exception. "Edge not found in graph"))))

(defn find-edge [ggraph start-vertex end-vertex]
  (gvertex/find-edge (get-vertex-by-key ggraph
                                        (gvertex/get-key start-vertex))
                     end-vertex))

(defn find-vertex-by-key [ggraph vertex-key]
  (vertex-key (:vertices ggraph)))

(defn get-weight [ggraph]
  (reduce #(+ %1 (:weight %2)) 0 (get-all-edges ggraph)))

(defn reverse [ggraph]
  (reduce #(->> %
                (delete-edge ggraph)
                gedge/reverse
                (add-edge ggraph))
          ggraph
          (get-all-edges ggraph)))

(defn get-vertices-indices [ggraph]
  (last (reduce (fn [[i vs] v]
                  [(inc i)
                   (assoc vs (gvertex/get-key v) i)])
                [0 {}]
                (get-all-vertices ggraph))))

(defn get-adjacency-matrix [ggraph]
  (let [vertices (get-all-vertices ggraph)
        vertices-indices (get-vertices-indices ggraph)
        adjacency-matrix (let [xs (range (count vertices))]
                           (map (fn [_] (map (fn [_] (Integer/MAX_VALUE)) xs)) xs))]
    (last (reduce (fn [[i vs] vertex]
                    [(inc i)
                     (reduce #(assoc-in %1
                                        [i ((gvertex/get-key %2) vertices-indices)]
                                        (:weight (find-edge ggraph vertex %2)))
                             vs
                             (gvertex/get-neighbors vertex))])
                  [0 adjacency-matrix]
                  vertices))))

(defn ->string [ggraph]
  (str (keys (:vertices ggraph))))
