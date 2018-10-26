(ns algorithm-data-structure.algorithms.graph.bellman-ford-test
  (:require [algorithm-data-structure.algorithms.graph.bellman-ford :as bf]
            [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [clojure.test :refer :all]))

(deftest run-test
  (let [graph (-> (g/create)
                  (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
                  (g/add-edge (ge/create (gv/create 2) (gv/create 3)))
                  (g/add-edge (ge/create (gv/create 3) (gv/create 4))))
        get-vertex #(-> graph
                        (g/get-vertex-by-key (-> (gv/create %) gv/get-key)))
        vertex (get-vertex 1)]
    (is (= {:distances {1 0 2 0 3 0 4 0}
            :previous-vertices {1 nil
                                2 (get-vertex 1)
                                3 (get-vertex 2)
                                4 (get-vertex 3)}}
           (bf/run graph vertex))))
  ;; FIXME: negative weight can't work
  (let [graph (-> (g/create)
                  (g/add-edge (ge/create (gv/create 1) (gv/create 2) 2))
                  (g/add-edge (ge/create (gv/create 2) (gv/create 3) 9))
                  (g/add-edge (ge/create (gv/create 3) (gv/create 4) 6)))
        get-vertex #(-> graph
                        (g/get-vertex-by-key (-> (gv/create %) gv/get-key)))
        vertex (get-vertex 1)]
    (is (= {:distances {1 0 2 2 3 11 4 17}
            :previous-vertices {1 nil
                                2 (get-vertex 1)
                                3 (get-vertex 2)
                                4 (get-vertex 3)}}
           (bf/run graph vertex)))))
