(ns algorithm-data-structure.algorithms.graph.depth-first-search
  (:require [algorithm-data-structure.data-structures.graph :as ggraph]
            [algorithm-data-structure.data-structures.graph-vertex :as gvertex]
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
  (let [previous-vertex nil
        rec (fn rec [current-vertex previous-vertex]
              (enter-vertex (->m current-vertex previous-vertex))
              (doseq [next-vertex (ggraph/get-neighbors ggraph current-vertex)]
                (if (allow-traversal (->m previous-vertex current-vertex next-vertex))
                  (rec next-vertex current-vertex)))
              (leave-vertex (->m current-vertex previous-vertex)))]
    (rec start-vertex previous-vertex)))
