(ns algorithm-data-structure.algorithms.graph.eulerian-path-test
  (:require [algorithm-data-structure.algorithms.graph.eulerian-path :as ep]
            [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [clojure.test :refer :all]))

(def graph (-> (g/create)
               (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
               (g/add-edge (ge/create (gv/create 2) (gv/create 3)))
               (g/add-edge (ge/create (gv/create 3) (gv/create 4)))
               (g/add-edge (ge/create (gv/create 4) (gv/create 5)))
               (g/add-edge (ge/create (gv/create 5) (gv/create 1)))

               (g/add-edge (ge/create (gv/create 1) (gv/create 3)))
               (g/add-edge (ge/create (gv/create 1) (gv/create 4)))
               (g/add-edge (ge/create (gv/create 2) (gv/create 4)))
               (g/add-edge (ge/create (gv/create 2) (gv/create 5)))
               (g/add-edge (ge/create (gv/create 3) (gv/create 5)))
               ))

(deftest run-test
  (is (= [1 ]
         (map #(gv/get-key %) (ep/run graph)))))
