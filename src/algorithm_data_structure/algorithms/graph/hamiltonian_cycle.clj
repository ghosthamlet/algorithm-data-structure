(ns algorithm-data-structure.algorithms.graph.hamiltonian-cycle
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]))

(defn safe? [adjacency-matrix vertices-indices cycle vertex-candidate]
  (let [end-vertex (cycle (dec (count cycle)))
        candidate-vertex-adjacency-index (vertices-indices (gv/get-key vertex-candidate))
        end-vertex-adjacency-index (vertices-indices (gv/get-key end-vertex))]
    (if (= (get-in adjacency-matrix
                   [end-vertex-adjacency-index candidate-vertex-adjacency-index])
           Double/POSITIVE_INFINITY)
      false
      (first (filter #(= (gv/get-key %) (gv/get-key vertex-candidate))
                     cycle)))))

(defn cycle? [adjacency-matrix vertices-indices cycle]
  (let [start-vertex (first cycle)
        end-vertex (cycle (dec (count cycle)))
        start-vertex-adjacency-index (vertices-indices (gv/get-key start-vertex))
        end-vertex-adjacency-index (vertices-indices (gv/get-key end-vertex))]
    (not= (get-in adjacency-matrix
                  [end-vertex-adjacency-index start-vertex-adjacency-index])
          Double/POSITIVE_INFINITY)))

(defn hamiltonian-cycle-recursive [adjacency-matrix vertices
                                   vertices-indices cycles cycle]
  (let [current-cycle cycle
        vlen (count vertices)]
    (if (= (count vertices) (count current-cycle))
      (if (cycle? adjacency-matrix vertices-indices current-cycle)
        (conj cycles current-cycle)
        cycles)
      (loop [vertex-index 0
             cycles cycles]
        (if (= vertex-index vlen)
          cycles
          (let [vertex-candidate (vertices vertex-index)]
            (recur (inc vertex-index)
                   (if (safe? adjacency-matrix vertices-indices
                              current-cycle vertex-candidate)
                     (hamiltonian-cycle-recursive adjacency-matrix vertices
                                                  vertices-indices cycles
                                                  (conj current-cycle vertex-candidate))
                     cycles))))))))

(defn run [graph]
  (let [vertices-indices (g/get-vertices-indices graph)
        adjacency-matrix (g/get-adjacency-matrix graph)
        vertices (g/get-all-vertices graph)
        start-vertex (vertices 0)]
    (hamiltonian-cycle-recursive adjacency-matrix vertices
                                 vertices-indices []
                                 [start-vertex])))
