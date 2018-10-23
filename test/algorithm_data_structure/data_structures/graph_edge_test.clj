(ns algorithm-data-structure.data-structures.graph-edge-test
  (:require [algorithm-data-structure.data-structures.graph-edge :as ge]
            [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :refer :all]))

(def vertex (gv/create 1))
(def edge (ge/create vertex (gv/create 2)))

(deftest create-test
  (is (= (-> {:start-vertex (gv/create 1)
              :end-vertex (gv/create 2)
              :weight 0}
             record->map)
         (-> edge
             record->map))))

(deftest get-key-test
  (is (= "1_2"
         (-> edge
             ge/get-key))))

(deftest reverse-test
  (is (= (-> {:start-vertex (gv/create 2)
              :end-vertex (gv/create 1)
              :weight 0}
             record->map)
         (-> edge
             ge/reverse
             record->map))))

(deftest ->string-test
  (is (= "1_2"
         (-> edge
             ge/->string))))
