(ns algorithm-data-structure.algorithms.graph.prim
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.priority-queue :as pq]))

(defn run [graph]
  (if (:directed? graph)
    (throw (Exception. "Prim's algorithms works only for undirected graphs"))
    (let [minimum-spanning-tree (atom (g/create))
          visited-vertices (atom {})
          start-vertex (first (g/get-all-vertices graph))
          _ (swap! visited-vertices assoc
                   (gv/get-key start-vertex) start-vertex)
          edges-queue (atom (reduce #(pq/add %1 %2 (:weight %2))
                                   (pq/create) (gv/get-edges start-vertex)))]
      (while (not (pq/empty? @edges-queue))
        (let [[_ current-min-edge] (swap! edges-queue pq/poll)
              next-min-vertex (cond
                                (visited-vertices (gv/get-key (:start-vertex current-min-edge)))
                                (:start-vertex current-min-edge)
                                (visited-vertices (gv/get-key (:end-vertex current-min-edge)))
                                (:end-vertex current-min-edge)
                                :else nil)]
          (when next-min-vertex
            (swap! minimum-spanning-tree g/add-edge current-min-edge)
            (swap! visited-vertices assoc
                   (gv/get-key next-min-vertex) next-min-vertex)
            (doseq [graph-edge (gv/get-edges next-min-vertex)]
              (when (or (not (@visited-vertices (gv/get-key (:start-vertex graph-edge))))
                        (not (@visited-vertices (gv/get-key (:end-vertex graph-edge)))))
                (swap! edges-queue pq/add graph-edge (:weight graph-edge)))))))
      @minimum-spanning-tree)))
