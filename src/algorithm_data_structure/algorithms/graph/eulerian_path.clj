(ns algorithm-data-structure.algorithms.graph.eulerian-path
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [algorithm-data-structure.algorithms.graph.bridges :as b]))

(defn run [graph]
  (let [eulerian-path-vertices (atom [])
        even-rank-vertices (atom {})
        odd-rank-vertices (atom {})
        not-visited-edges (atom {})]
    (doseq [vertex (g/get-all-edges graph)]
      (swap! not-visited-edges assoc
             (gv/get-key vertex) vertex))
    (doseq [vertex (g/get-all-vertices graph)]
      (if (not= 0 (mod (gv/get-degree) 2))
        (swap! odd-rank-vertices assoc
               (gv/get-key vertex) vertex)
        (swap! even-rank-vertices assoc
               (gv/get-key vertex) vertex)))
    (let [circuit? (zero? (count @odd-rank-vertices))]
      (if (and (not circuit?) (not= 2 (count @odd-rank-vertices)))
        (throw (Exception. "Eulerian path must contain two odd-ranked vertices"))
        (let [start-vertex (if circuit?
                             (->> @even-rank-vertices keys first @even-rank-vertices)
                             (->> @odd-rank-vertices keys first @odd-rank-vertices))]
          (loop [current-vertex start-vertex
                 graph graph]
            (if (pos? (count @not-visited-edges))
              @eulerian-path-vertices
              (let [bridges (b/run graph)
                    current-edges (gv/get-edges current-vertex)
                    edge-to-delete (first (if (= 1 (count current-edges))
                                            current-edges
                                            (filter #(not (bridges (ge/get-key %))) current-edges)))]
                (swap! eulerian-path-vertices conj
                       current-vertex)
                (let [current-vertex (if (= (gv/get-key current-vertex)
                                            (gv/get-key (:start-vertex edge-to-delete)))
                                       (:end-vertex edge-to-delete)
                                       (:start-vertex edge-to-delete))]
                  (swap! not-visited-edges dissoc
                         (gv/get-key edge-to-delete))
                  (when (zero? (count @not-visited-edges))
                    (swap! eulerian-path-vertices conj
                           current-vertex))
                  (recur current-vertex
                         (g/delete-edge graph edge-to-delete)))))))))))
