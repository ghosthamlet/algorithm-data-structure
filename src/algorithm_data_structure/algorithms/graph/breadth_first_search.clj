(ns algorithm-data-structure.algorithms.graph.breadth-first-search
  (:require [algorithm-data-structure.data-structures.graph :as ggraph]
            [algorithm-data-structure.data-structures.graph-vertex :as gvertex]
            [algorithm-data-structure.data-structures.queue :as queue]
            [algorithm-data-structure.util :refer :all]))

(defn stub-callback [x])

(def allow-traversal
  (let [seen (atom {})]
    (fn [{next-vertex :next-vertex}]
      (let [k (gvertex/get-key next-vertex)]
        (if (@seen k)
          false
          (do (reset! seen k true)
              true))))))

(defn run [ggraph start-vertex
           & {:keys [allow-traversal enter-vertex leave-vertex]
              :or {allow-traversal allow-traversal
                   enter-vertex stub-callback
                   leave-vertex stub-callback}
              :as callbacks}]
  (let [vertex-queue (queue/enqueue (queue/create) start-vertex)]
    (loop [previous-vertex nil
           [vertex-queue current-vertex] (queue/dequeue vertex-queue)]
      (let [_ (enter-vertex (->m current-vertex previous-vertex))
            vertex-queue (reduce #(if (allow-traversal (->m previous-vertex current-vertex %2))
                                    (queue/enqueue %1 %2)
                                    %1)
                                 vertex-queue
                                 (ggraph/get-neighbors ggraph current-vertex))
            _ (leave-vertex (->m current-vertex previous-vertex))]
        (when-not (queue/empty? vertex-queue)
          (recur current-vertex
                 (queue/dequeue vertex-queue)))))))
