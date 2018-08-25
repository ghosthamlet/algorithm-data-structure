(ns algorithm-data-structure.algorithms.graph.detect-undirected-cycle
  (:require [algorithm-data-structure.algorithms.graph.depth-first-search :as dfs]
            [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]))

(defn run [graph]
  (let [cycle (atom nil)
        visited-vertices (atom {})
        parents (atom {})
        callbacks {:allow-traversal
                   (fn [{:keys [current-vertex next-vertex]}]
                     (if @cycle
                       false
                       (let [current-vertex-parent (@parents (gv/get-key current-vertex))]
                         (not= (when current-vertex-parent
                                 (gv/get-key current-vertex-parent))
                               (gv/get-key next-vertex)))))
                   :enter-vertex
                   (fn [{:keys [current-vertex previous-vertex]}]
                     (let [current-key (gv/get-key current-vertex)]
                       (if (@visited-vertices current-key)
                         (do
                           (reset! cycle {})
                           (loop [current-cycle-vertex current-vertex
                                  previous-cycle-vertex previous-vertex]
                             (if (= (gv/get-key previous-cycle-vertex)
                                    current-key)
                               (swap! cycle assoc
                                      (gv/get-key current-cycle-vertex) previous-cycle-vertex)
                               (do
                                 (swap! cycle assoc
                                        (gv/get-key current-cycle-vertex) previous-cycle-vertex)
                                 (recur previous-cycle-vertex
                                        (@parents (gv/get-key previous-cycle-vertex)))))))
                         (do
                           (swap! visited-vertices assoc
                                  current-key current-vertex)
                           (swap! parents assoc
                                  current-key previous-vertex)))))}]
    (dfs/run graph (first (g/get-all-vertices graph)) callbacks)
    @cycle))
