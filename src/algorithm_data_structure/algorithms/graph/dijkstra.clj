(ns algorithm-data-structure.algorithms.graph.dijkstra
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/algorithms/graph/dijkstra"
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.priority-queue :as pq]
            [algorithm-data-structure.data-structures.min-heap :as mh]
            [algorithm-data-structure.util :refer :all]))

(defn run [graph start-vertex]
  (let [[distances previous-vertices] (reduce (fn [[d p] v]
                                                [(assoc d (gv/get-key v) Integer/MAX_VALUE)
                                                 (assoc p (gv/get-key v) nil)])
                                              [{} {}]
                                              (g/get-all-vertices graph))
        distances (assoc distances (gv/get-key start-vertex) 0)
        queue (pq/add (pq/create)
                      start-vertex
                      (distances (gv/get-key start-vertex)))]
    (loop [queue queue
           distances distances
           previous-vertices previous-vertices
           visited-vertices {}]
      (if (mh/empty?* queue)
        {:distances distances
         :previous-vertices previous-vertices}
        (let [[queue current-vertex] (pq/poll queue)
              current-key (gv/get-key current-vertex)
              current-vertex (g/get-vertex-by-key graph current-key)
              [queue
               distances
               previous-vertices] (reduce (fn [[queue distances previous-vertices] neighbor]
                                            (let [neighbor-key (gv/get-key neighbor)]
                                              (if (visited-vertices neighbor-key)
                                                [queue distances previous-vertices]
                                                (let [edge (g/find-edge graph current-vertex neighbor)
                                                      existing-distance-to-neighbor (distances neighbor-key)
                                                      distance-to-neighbor-from-current (+ (distances current-key) (:weight edge))
                                                      [queue
                                                       distances
                                                       previous-vertices] (if (< distance-to-neighbor-from-current existing-distance-to-neighbor)
                                                                            [(if (pq/has-value queue neighbor)
                                                                               (pq/change-priority queue neighbor (distances neighbor-key))
                                                                               queue)
                                                                             (assoc distances neighbor-key distance-to-neighbor-from-current)
                                                                             (assoc previous-vertices neighbor-key current-vertex)]
                                                                            [queue distances previous-vertices])]
                                                  [(if (pq/has-value queue neighbor)
                                                     queue
                                                     (pq/add queue neighbor (distances neighbor-key)))
                                                   distances
                                                   previous-vertices]))))
                                          [queue distances previous-vertices]
                                          (g/get-neighbors graph current-vertex))]
          (recur queue
                 distances
                 previous-vertices
                 (assoc visited-vertices current-key current-vertex)))))))

(defn run-with-local-mutable
  "much more simple than immutable version"
  [graph start-vertex]
  (let [distances (atom {})
        previous-vertices (atom {})
        visited-vertices (atom {})
        queue (do (doseq [v (g/get-all-vertices graph)]
                    (swap! distances assoc (gv/get-key v) Integer/MAX_VALUE)
                    (swap! previous-vertices assoc (gv/get-key v) nil))
                  (swap! distances assoc (gv/get-key start-vertex) 0)
                  (pq/add (pq/create)
                          start-vertex
                          (@distances (gv/get-key start-vertex))))]
    (loop [queue queue]
      (if (mh/empty?* queue)
        {:distances @distances
         :previous-vertices @previous-vertices}
        (let [[queue current-vertex] (pq/poll queue)
              current-key (gv/get-key current-vertex)
              current-vertex (g/get-vertex-by-key graph current-key)]
          (swap! visited-vertices assoc current-key current-vertex)
          (recur (reduce (fn [queue neighbor]
                           (let [neighbor-key (gv/get-key neighbor)]
                             (if (@visited-vertices neighbor-key)
                               queue
                               (let [edge (g/find-edge graph current-vertex neighbor)
                                     existing-distance-to-neighbor (@distances neighbor-key)
                                     distance-to-neighbor-from-current (+ (@distances current-key) (:weight edge))
                                     queue (if (< distance-to-neighbor-from-current existing-distance-to-neighbor)
                                             (do (swap! distances assoc neighbor-key distance-to-neighbor-from-current)
                                                 (swap! previous-vertices assoc neighbor-key current-vertex)
                                                 (if (pq/has-value queue neighbor)
                                                   (pq/change-priority queue neighbor (@distances neighbor-key))
                                                   queue))
                                             queue)]
                                 (if (pq/has-value queue neighbor)
                                   queue
                                   (pq/add queue neighbor (@distances neighbor-key)))))))
                         queue
                         (g/get-neighbors graph current-vertex))))))))
