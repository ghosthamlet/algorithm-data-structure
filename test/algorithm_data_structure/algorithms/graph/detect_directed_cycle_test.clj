(ns algorithm-data-structure.algorithms.graph.detect-directed-cycle-test
  (:require [algorithm-data-structure.algorithms.graph.detect-directed-cycle :as ddc]
            [algorithm-data-structure.data-structures.graph :as g]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [clojure.test :refer :all]))

(def graph (-> (g/create true)
               (g/add-edge (ge/create (gv/create 1) (gv/create 2)))
               (g/add-edge (ge/create (gv/create 2) (gv/create 3)))
               (g/add-edge (ge/create (gv/create 3) (gv/create 4)))))

(deftest run-test
  (is (= nil
         (ddc/run graph)))
  (is (= [1 2 3 4 5 6]
         (-> graph
             (g/add-edge (ge/create (gv/create 4) (gv/create 5)))
             (g/add-edge (ge/create (gv/create 5) (gv/create 6)))
             (g/add-edge (ge/create (gv/create 6) (gv/create 1)))
             ddc/run
             keys
             sort))))

