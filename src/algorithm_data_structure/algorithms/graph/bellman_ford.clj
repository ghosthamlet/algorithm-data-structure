(ns algorithm-data-structure.algorithms.graph.bellman-ford
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/algorithms/graph/bellman-ford"
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]))

(defn run [graph start-vertex]
  (let [start-key (gv/get-key start-vertex)
        distances (atom (assoc {} start-key 0))
        previous-vertices (atom {})
        all-vertices (g/get-all-vertices graph)]
    (doseq [vertex all-vertices]
      (let [key (gv/get-key vertex)]
        (swap! previous-vertices assoc key nil)
        (when (not= key start-key)
          (swap! distances assoc key Double/POSITIVE_INFINITY))))
    (dotimes [_ (dec (count all-vertices))]
      (doseq [vertex-key (keys @distances)
              :let [vertex (g/get-vertex-by-key graph vertex-key)]
              neighbor (g/get-neighbors graph vertex)
              :let [edge (g/find-edge graph vertex neighbor)
                    distance->vertex (@distances (gv/get-key vertex))
                    distance->neighbor (+ distance->vertex (:weight edge))
                    neighbor-key (gv/get-key neighbor)]]
        (when (< distance->neighbor (@distances neighbor-key))
          (swap! distances assoc neighbor-key distance->neighbor)
          (swap! previous-vertices assoc neighbor-key vertex))))
    {:distances @distances
     :previous-vertices @previous-vertices}))
