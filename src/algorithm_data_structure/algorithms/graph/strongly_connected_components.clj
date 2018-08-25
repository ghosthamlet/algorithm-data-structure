(ns algorithm-data-structure.algorithms.graph.strongly-connected-components
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.algorithms.graph.depth-first-search :as dfs]
            [algorithm-data-structure.data-structures.stack :as st]))

(defn get-vertices-sorted-by-dfs-finish-time [graph]
  (let [visited-vertices-set (atom {})
        vertices-by-dfs-finish-time (atom (st/create))
        not-visited-vertices-set (atom (reduce #(assoc %1 (gv/get-key %2) %2)
                                               {} (g/get-all-vertices graph)))
        dfs-callback {:enter-vertex
                      (fn [{:keys [current-vertex]}]
                        (swap! visited-vertices-set assoc
                               (gv/get-key current-vertex) current-vertex)
                        (swap! not-visited-vertices-set dissoc
                               (gv/get-key current-vertex)))
                      :leave-vertex
                      (fn [{:keys [current-vertex]}]
                        (swap! vertices-by-dfs-finish-time st/push
                               current-vertex))
                      :allow-traversal
                      (fn [{:keys [next-vertex]}]
                        (not (@visited-vertices-set (gv/get-key next-vertex))))}]
    (while (seq @not-visited-vertices-set)
      (let [start-vertex-key (first (keys @not-visited-vertices-set))
            start-vertex (@not-visited-vertices-set start-vertex-key)]
        (swap! not-visited-vertices-set dissoc
               start-vertex-key)
        (dfs/run graph start-vertex dfs-callback)))
    @vertices-by-dfs-finish-time))

(defn get-scc-sets [graph vertices-by-finish-time]
  (let [strongly-connected-components-sets (atom [])
        strongly-connected-components-set (atom [])
        visited-vertices-set (atom {})
        dfs-callback {:enter-vertex
                      (fn [{:keys [current-vertex]}]
                        (swap! strongly-connected-components-set conj
                               current-vertex)
                        (swap! visited-vertices-set assoc
                               (gv/get-key current-vertex) current-vertex))
                      :leave-vertex
                      (fn [{:keys [previous-vertex]}]
                        (when (nil? previous-vertex)
                          (swap! strongly-connected-components-sets conj
                                 strongly-connected-components-set)))
                      :allow-traversal
                      (fn [{:keys [next-vertex]}]
                        (not (@visited-vertices-set (gv/get-key next-vertex))))}]
    (while (not (st/empty? vertices-by-finish-time))
      (let [[_ start-vertex] (swap! vertices-by-finish-time st/pop)]
        (reset! strongly-connected-components-set [])
        (when-not (@visited-vertices-set (gv/get-key start-vertex))
          (dfs/run graph start-vertex dfs-callback))))
    @strongly-connected-components-sets))

(defn run [graph]
  (let [vertices-by-finish-time (get-vertices-sorted-by-dfs-finish-time graph)]
    (get-scc-sets (g/reverse graph) vertices-by-finish-time)))
