(ns algorithm-data-structure.algorithms.graph.bellman-ford
  (:require [algorithm-data-structure.data-structures.graph :as ggraph]
            [algorithm-data-structure.data-structures.graph-vertex :as gvertex]))

(defn run [ggraph start-vertex]
  (let [start-key (gvertex/get-key start-vertex)
        distances (atom (assoc {} start-key 0))
        previous-vertices (atom {})
        all-vertices (ggraph/get-all-vertices ggraph)]
    (doseq [vertex all-vertices]
      (let [key (gvertex/get-key vertex)]
        (swap! previous-vertices assoc key nil)
        (when (not= key start-key)
          (swap! distances assoc key Double/POSITIVE_INFINITY))))
    (dotimes [_ (dec (count all-vertices))]
      (doseq [vertex-key (keys @distances)]
        (let [vertex (ggraph/get-vertex-by-key vertex-key)]
          (doseq [neighbor (ggraph/get-neighbors ggraph vertex)]
            (let [edge (ggraph/find-edge ggraph vertex neighbor)
                  distance->vertex (distances (gvertex/get-key vertex))
                  distance->neighbor (+ distance->vertex (:weight edge))
                  neighbor-key (gvertex/get-key neighbor)]
              (when (< distance->neighbor (distances neighbor-key))
                (swap! distances assoc neighbor-key distance->neighbor)
                (swap! previous-vertices assoc neighbor-key vertex)))))))
    {:distances @distances
     :previous-vertices @previous-vertices}))
