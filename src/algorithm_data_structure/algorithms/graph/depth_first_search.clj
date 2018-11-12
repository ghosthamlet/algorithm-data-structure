(ns algorithm-data-structure.algorithms.graph.depth-first-search
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/algorithms/graph/depth-first-search"
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
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
  (allow-traversal {:next-vertex start-vertex})
  (let [previous-vertex nil
        ;; XXX: don't save current-vertex, it may no edges data, instead seve key, get vertex from graph by key
        rec (fn rec [current-vertex-key previous-vertex-key]
              (let [current-vertex (g/get-vertex-by-key graph current-vertex-key)
                    previous-vertex (g/get-vertex-by-key graph previous-vertex-key)]
                (enter-vertex (->m current-vertex previous-vertex))
                (doseq [next-vertex (g/get-neighbors graph current-vertex)]
                  (if (allow-traversal (->m previous-vertex current-vertex next-vertex))
                    (rec (gv/get-key next-vertex) current-vertex-key)))
                (leave-vertex (->m current-vertex previous-vertex))))]
    (rec (gv/get-key start-vertex) (gv/get-key previous-vertex))))
