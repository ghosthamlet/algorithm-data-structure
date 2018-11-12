(ns algorithm-data-structure.algorithms.graph.detect-directed-cycle
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/algorithms/graph/detect-cycle"
  (:require [algorithm-data-structure.algorithms.graph.depth-first-search :as dfs]
            [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]))

(defn run [graph]
  (let [cycle (atom nil)
        dfs-parent-map (atom {})
        white-set (atom {})
        gray-set (atom {})
        black-set (atom {})
        enter-vertex (fn [{:keys [current-vertex previous-vertex]}]
                       (let [current-key (gv/get-key current-vertex)]
                         (if (@gray-set current-key)
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
                                          (@dfs-parent-map (gv/get-key previous-cycle-vertex)))))))
                           (do
                             (swap! gray-set assoc
                                    current-key current-vertex)
                             (swap! white-set dissoc
                                    current-key)
                             (swap! dfs-parent-map assoc
                                    current-key previous-vertex)))))
        leave-vertex (fn [{:keys [current-vertex]}]
                       (swap! black-set assoc
                              (gv/get-key current-vertex) current-vertex)
                       (swap! gray-set dissoc
                              (gv/get-key current-vertex)))
        allow-traversal (fn [{:keys [next-vertex]}]
                          (if @cycle
                            false
                            (not (@black-set (gv/get-key next-vertex)))))]
    (doseq [vertex (g/get-all-vertices graph)]
      (swap! white-set assoc (gv/get-key vertex) vertex))
    (while (not-empty @white-set)
      (dfs/run graph
        (->> @white-set keys first (get @white-set))
        :enter-vertex enter-vertex
        :leave-vertex leave-vertex
        :allow-traversal allow-traversal))
    @cycle))
