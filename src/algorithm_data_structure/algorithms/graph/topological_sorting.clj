(ns algorithm-data-structure.algorithms.graph.topological-sorting
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.stack :as st]
            [algorithm-data-structure.algorithms.graph.depth-first-search :as dfs]))

(defn run [graph]
  (let [unvisited-set (atom (reduce #(assoc %1 (gv/get-key %2) %2)
                                    {} (g/get-all-vertices graph)))
        visited-set (atom {})
        sorted-stack (atom (st/create))
        dfs-callback {:enter-vertex
                      (fn [{:keys [current-vertex]}]
                        (swap! visited-set assoc
                               (gv/get-key current-vertex) current-vertex)
                        (swap! unvisited-set dissoc
                               (gv/get-key current-vertex)))
                      :leave-vertex
                      (fn [{:keys [current-vertex]}]
                        (swap! sorted-stack st/push
                               current-vertex))
                      :allow-traversal
                      (fn [{:keys [next-vertex]}]
                        (not (@visited-set (gv/get-key next-vertex))))}]
    (while (seq @unvisited-set)
      (let [current-vertex-key (first (keys @unvisited-set))
            current-vertex (@unvisited-set current-vertex-key)]
        (dfs/run graph current-vertex dfs-callback)))
    (st/->array @sorted-stack)))
