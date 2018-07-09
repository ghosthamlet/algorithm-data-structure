(ns algorithm-data-structure.algorithms.graph.dijkstra
  (:require [algorithm-data-structure.data-structures.graph :as ggraph]
            [algorithm-data-structure.data-structures.graph-vertex :as gvertex]
            [algorithm-data-structure.data-structures.priority-queue :as queue]
            [algorithm-data-structure.util :refer :all]))

(defn search [ggraph start-vertex]
  (let [[distances previous-vertices] (reduce (fn [[d p] v]
                                                [(assoc d (gvertex/get-key v) Integer/MAX_VALUE)
                                                 (assoc p (gvertex/get-key v) nil)])
                                              [{} {}]
                                              (ggraph/get-all-vertices ggraph))
        distances (assoc distances (gvertex/get-key start-vertex) 0)
        queue (queue/add (queue/create)
                         start-vertex
                         ((gvertex/get-key start-vertex) distances))]
    (loop [queue queue
           distances distances
           previous-vertices previous-vertices
           visited-vertices {}]
      (if (queue/empty? queue)
        {:distances distances
         :previous-vertices previous-vertices}
        (let [[queue current-vertex] (queue/poll queue)
              [queue
               distances
               previous-vertices] (reduce (fn [[queue distances previous-vertices] neighbor]
                                            (let [neighbor-key (gvertex/get-key neighbor)
                                                  current-vertex-key (gvertex/get-key current-vertex)]
                                              (if (neighbor-key visited-vertices)
                                                [queue distances previous-vertices]
                                                (let [edge (ggraph/find-edge ggraph current-vertex neighbor)
                                                      existing-distance-to-neighbor (neighbor-key distances)
                                                      distance-to-neighbor-from-current (+ (current-vertex-key distances) (:weight edge))
                                                      [queue
                                                       distances
                                                       previous-vertices] (if (< distance-to-neighbor-from-current existing-distance-to-neighbor)
                                                                            [(if (queue/has-value neighbor)
                                                                               (queue/change-priority queue neighbor (neighbor-key distances))
                                                                               queue)
                                                                             (assoc distances neighbor-key distance-to-neighbor-from-current)
                                                                             (assoc previous-vertices neighbor-key current-vertex)]
                                                                            [queue distances previous-vertices])]
                                                  [(if (queue/has-value queue neighbor)
                                                     queue
                                                     (queue/add queue neighbor (neighbor-key distances)))
                                                   distances
                                                   previous-vertices]))))
                                          [queue distances previous-vertices]
                                          (ggraph/get-neighbors ggraph current-vertex))]
          (recur queue
                 distances
                 previous-vertices
                 (assoc visited-vertices (gvertex/get-key current-vertex) current-vertex)))))))

(defn search-with-local-mutable
  "much more simple than pure fn version"
  [ggraph start-vertex]
  (let [distances (atom {})
        previous-vertices (atom {})
        visited-vertices (atom {})
        queue (do (doseq [v (ggraph/get-all-vertices ggraph)]
                    (swap! distances assoc (gvertex/get-key v) Integer/MAX_VALUE)
                    (swap! previous-vertices assoc (gvertex/get-key v) nil))
                  (swap! distances assoc (gvertex/get-key start-vertex) 0)
                  (queue/add (queue/create)
                             start-vertex
                             ((gvertex/get-key start-vertex) @distances)))]
    (loop [queue queue]
      (if (queue/empty? queue)
        {:distances @distances
         :previous-vertices @previous-vertices}
        (let [[queue current-vertex] (queue/poll queue)]
          (swap! visited-vertices assoc (gvertex/get-key current-vertex) current-vertex)
          (recur (reduce (fn [queue neighbor]
                           (let [neighbor-key (gvertex/get-key neighbor)
                                 current-vertex-key (gvertex/get-key current-vertex)]
                             (if (neighbor-key @visited-vertices)
                               queue
                               (let [edge (ggraph/find-edge ggraph current-vertex neighbor)
                                     existing-distance-to-neighbor (neighbor-key @distances)
                                     distance-to-neighbor-from-current (+ (current-vertex-key @distances) (:weight edge))
                                     queue (if (< distance-to-neighbor-from-current existing-distance-to-neighbor)
                                             (do (swap! distances assoc neighbor-key distance-to-neighbor-from-current)
                                                 (swap! previous-vertices assoc neighbor-key current-vertex)
                                                 (if (queue/has-value neighbor)
                                                   (queue/change-priority queue neighbor (neighbor-key @distances))
                                                   queue))
                                             queue)]
                                 (if (queue/has-value queue neighbor)
                                   queue
                                   (queue/add queue neighbor (neighbor-key @distances)))))))
                         queue
                         (ggraph/get-neighbors ggraph current-vertex))))))))
