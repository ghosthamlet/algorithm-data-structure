(ns algorithm-data-structure.data-structures.graph-vertex-test
  (:require [algorithm-data-structure.data-structures.graph-vertex :as gv]
            [algorithm-data-structure.data-structures.graph-edge :as ge]
            [algorithm-data-structure.data-structures.linked-list :as ll]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :refer :all]))

(def vertex (gv/create 1))
(def edge (ge/create vertex vertex))

(deftest create-test
  (is (= {:value 1
          :edges (-> (ll/create)
                     record->map)}
         (-> vertex
             record->map))))

(deftest get-key-test
  (is (= 1
         (-> vertex
             gv/get-key))))

(deftest add-edge-test
  (is (= {:value 1
          :edges (-> (ll/create)
                     (ll/append edge)
                     record->map)}
         (-> vertex
             (gv/add-edge edge)
             record->map)))
  (is (= {:value 1
          :edges (-> (ll/create)
                     (ll/append edge)
                     (ll/append edge)
                     record->map)}
         (-> vertex
             (gv/add-edge edge)
             (gv/add-edge edge)
             record->map))))

(deftest delete-edge-test
  (is (= {:value 1
          :edges (-> (ll/create)
                     (ll/append edge)
                     record->map)}
         (-> vertex
             (gv/add-edge edge)
             (gv/delete-edge {})
             record->map)))
  (is (= {:value 1
          :edges (-> (ll/create)
                     record->map)}
         (-> vertex
             (gv/add-edge edge)
             (gv/delete-edge edge)
             record->map)))
  (is (= {:value 1
          :edges (-> (ll/create)
                     record->map)}
         (-> vertex
             (gv/add-edge edge)
             (gv/add-edge edge)
             (gv/delete-edge edge)
             record->map))))

(deftest get-neighbors-test
  (is (= []
         (-> vertex
             gv/get-neighbors
             record->map)))
  (is (= (-> [(gv/create 2) (gv/create 3)]
             record->map)
         (-> vertex
             (gv/add-edge (ge/create vertex (gv/create 2)))
             (gv/add-edge (ge/create (gv/create 3) vertex))
             gv/get-neighbors
             record->map))))

(deftest get-edges-test
  (is (= []
         (-> vertex
             gv/get-edges
             record->map)))
  (is (= (-> [(ge/create vertex (gv/create 2))
              (ge/create (gv/create 3) vertex)]
             record->map)
         (-> vertex
             (gv/add-edge (ge/create vertex (gv/create 2)))
             (gv/add-edge (ge/create (gv/create 3) vertex))
             gv/get-edges
             record->map))))

(deftest get-degree-test
  (is (= 0
         (-> vertex
             gv/get-degree)))
  (is (= 2
         (-> vertex
             (gv/add-edge (ge/create vertex (gv/create 2)))
             (gv/add-edge (ge/create (gv/create 3) vertex))
             gv/get-degree))))

(deftest has-edge-test
  (is (= false
         (-> vertex
             (gv/has-edge (ge/create vertex (gv/create 2))))))
  (is (= true
         (-> vertex
             (gv/add-edge (ge/create vertex (gv/create 2)))
             (gv/add-edge (ge/create (gv/create 3) vertex))
             (gv/has-edge (ge/create vertex (gv/create 2)))))))

(deftest has-neighbor-test
  (is (= false
         (-> vertex
             (gv/has-neighbor (gv/create 2)))))
  (is (= true
         (-> vertex
             (gv/add-edge (ge/create vertex (gv/create 2)))
             (gv/add-edge (ge/create (gv/create 3) vertex))
             (gv/has-neighbor (gv/create 3))))))

(deftest find-edge-test
  (is (= nil
         (-> vertex
             (gv/find-edge (gv/create 2)))))
  (is (= (ge/create (gv/create 3) vertex)
         (-> vertex
             (gv/add-edge (ge/create vertex (gv/create 2)))
             (gv/add-edge (ge/create (gv/create 3) vertex))
             (gv/find-edge (gv/create 3))))))

(deftest delete-all-edges-test
  (is (= vertex
         (-> vertex
             gv/delete-all-edges)))
  (is (= vertex
         (-> vertex
             (gv/add-edge (ge/create vertex (gv/create 2)))
             (gv/add-edge (ge/create (gv/create 3) vertex))
             gv/delete-all-edges))))

(deftest ->string-test
  (is (= "1"
         (-> vertex
             (gv/->string nil)))))
