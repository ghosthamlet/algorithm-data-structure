(ns algorithm-data-structure.algorithms.graph.breadth-first-search
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/algorithms/graph/breadth-first-search"
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.queue :as q]
            [algorithm-data-structure.util :refer :all]))

(defn stub-callback [x])

(defn allow-traversal-fn []
  (let [seen (atom {})]
    (fn [{next-vertex :next-vertex}]
      (let [k (gv/get-key next-vertex)]
        (if (@seen k)
          false
          (do (swap! seen assoc k true)
              true))))))

(defn run [graph start-vertex
           & {:keys [allow-traversal enter-vertex leave-vertex]
              :or {allow-traversal (allow-traversal-fn)
                   enter-vertex stub-callback
                   leave-vertex stub-callback}
              :as callbacks}]
  (let [vertex-queue (q/enqueue (q/create) (gv/get-key start-vertex))]
    (allow-traversal {:next-vertex start-vertex})
    (loop [previous-vertex nil
           [vertex-queue current-vertex-key] (q/dequeue vertex-queue)]
      ;; XXX: don't save current-vertex, it may no edges data, instead seve key, get vertex from graph by key
      (let [current-vertex (g/get-vertex-by-key graph current-vertex-key)]
        (enter-vertex (->m current-vertex previous-vertex))
        (let [vertex-queue (reduce (fn [acc next-vertex]
                                     (if (allow-traversal (->m previous-vertex current-vertex next-vertex))
                                       (q/enqueue acc (gv/get-key next-vertex))
                                       acc))
                                   vertex-queue
                                   ;; XXX: neighbor vertex on vertex edge may no edges data, can only use its key
                                   (g/get-neighbors graph current-vertex))]
          (leave-vertex (->m current-vertex previous-vertex))
          (when-not (q/empty?* vertex-queue)
            (recur current-vertex
                   (q/dequeue vertex-queue))))))))
