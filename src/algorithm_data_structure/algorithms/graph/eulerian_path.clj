(ns algorithm-data-structure.algorithms.graph.eulerian-path
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/algorithms/graph/eulerian-path"
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [algorithm-data-structure.algorithms.graph.bridges :as b]))

(declare get-opposite-vertex)

(defn run [graph]
  (let [eulerian-path-vertices (atom [])
        even-rank-vertices (atom {})
        odd-rank-vertices (atom {})
        not-visited-edges (atom {})]
    (doseq [vertex (g/get-all-edges graph)]
      (swap! not-visited-edges assoc
             (gv/get-key vertex) vertex))
    (doseq [vertex (g/get-all-vertices graph)]
      (prn (gv/get-degree vertex))
      (if (not= 0 (mod (gv/get-degree vertex) 2))
        (swap! odd-rank-vertices assoc
               (gv/get-key vertex) vertex)
        (swap! even-rank-vertices assoc
               (gv/get-key vertex) vertex)))
    (let [circuit? (zero? (count @odd-rank-vertices))]
      (if (and (not circuit?) (not= 2 (count @odd-rank-vertices)))
        (throw (Exception. "Eulerian path must contain two odd-ranked vertices"))
        (let [start-vertex (if circuit?
                             (->> @even-rank-vertices keys sort first @even-rank-vertices)
                             (->> @odd-rank-vertices keys sort first @odd-rank-vertices))]
          (loop [current-vertex start-vertex
                 graph graph]
            (swap! eulerian-path-vertices
                   conj current-vertex)
            (if (zero? (count @not-visited-edges))
              @eulerian-path-vertices
              (let [bridges (b/run graph)
                    current-edges (gv/get-edges current-vertex)
                    edge-to-delete (first (if (= 1 (count current-edges))
                                            current-edges
                                            (filter #(not (bridges (ge/get-key %))) current-edges)))]
                (swap! not-visited-edges
                       dissoc (gv/get-key edge-to-delete))
                (recur (get-opposite-vertex current-vertex edge-to-delete)
                       (g/delete-edge graph edge-to-delete))))))))))

(defn- get-opposite-vertex [vertex edge]
  (if (= (gv/get-key vertex)
         (gv/get-key (:start-vertex edge)))
    (:end-vertex edge)
    (:start-vertex edge)))
