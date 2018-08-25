(ns algorithm-data-structure.algorithms.graph.kruskal
  (:require [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.disjoint-set :as ds]
            [algorithm-data-structure.algorithms.sorting.quck-sort :as qs]))

(defn run [graph]
  (if (:directed? graph)
    (throw (Exception. "Prim's algorithms works only for undirected graphs"))
    (let [sorting-callbacks {:compare-callback
                             (fn [graph-edge-a graph-edge-b]
                               (if (= (:weight graph-edge-a)
                                      (:weight graph-edge-b))
                                 1
                                 (if (<= (:weight graph-edge-a)
                                         (:weight graph-edge-b))
                                   -1 1)))}
          sorted-edges (qs/run (g/get-all-edges graph) sorting-callbacks)
          slen (count sorted-edges)]
      (loop [edge-index 0
             disjoint-set (reduce (fn [acc v]
                                    (ds/make-set acc v))
                                  (ds/create #(gv/get-key %)) (g/get-all-vertices graph))
             minimum-spanning-tree (g/create)]
        (if (= slen edge-index)
          minimum-spanning-tree
          (let [current-edge (sorted-edges edge-index)
                [disjoint-set minimum-spanning-tree]
                (if (ds/in-same-set (:start-vertex current-edge)
                                    (:end-vertex current-edge))
                  [disjoint-set minimum-spanning-tree]
                  [(ds/union disjoint-set
                             (:start-vertex current-edge)
                             (:end-vertex current-edge))
                   (g/add-edge minimum-spanning-tree current-edge)])]
            (recur (inc edge-index)
                   disjoint-set
                   minimum-spanning-tree)))))))
