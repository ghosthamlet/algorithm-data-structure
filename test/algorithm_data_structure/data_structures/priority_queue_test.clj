(ns algorithm-data-structure.data-structures.priority-queue-test
  (:require [algorithm-data-structure.data-structures.priority-queue :as queue]
            [algorithm-data-structure.util :refer :all]
            [clojure.test :as t]))

(def queue {:heap {:heap-container []}
            :priorities {}})
(def queue2 {:heap {:heap-container [-21 -20 0 -8 6 20 0 300 10 20]}
             :priorities {}})

(deftest create-test
  (is (= queue
         (queue/create))))

