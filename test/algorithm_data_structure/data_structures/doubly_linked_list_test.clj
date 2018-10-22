(ns algorithm-data-structure.data-structures.doubly-linked-list-test
  (:require [algorithm-data-structure.data-structures.doubly-linked-list :as dll]
            [clojure.test :refer :all])
  (:import [algorithm_data_structure.data_structures.doubly_linked_list DoublyLinkedList]))

(def linked (dll/create))

(defn eq-value [m coll]
  (let [collb (map #(:value (second %)) m)]
    (and (= (count coll) (count collb))
         (every? (fn [x] (some #(= x %) coll))
                 collb)
         (every? (fn [x] (some #(= x %) collb))
                 coll))))

(deftest eq-value-test
  (is (= false
         (eq-value {:a {:value 1}
                    :b {:value 2}
                    :c {:value 1}}
                   [2 1 0])))
  (is (= false
         (eq-value {:a {:value 1}
                    :b {:value 2}
                    :c {:value 1}}
                   [2 1])))
  (is (= true
         (eq-value {:a {:value 1}
                    :b {:value 2}
                    :c {:value 1}}
                   [2 1 1])))
  (is (= true
         (eq-value {:a {:value 1}
                    :b {:value 2}
                    :c {:value 1}}
                   [1 1 2]))))

(deftest create-test
  (is (= (DoublyLinkedList. nil nil nil)
         linked))
  (let [coll [2 1 3 6 1 9]
        linked (-> coll dll/create)]
    (is (= true
           (eq-value (.m linked) coll)))
    (is (= 2
           (:value ((.m linked) (.head linked)))))
    (is (= 9
           (:value ((.m linked) (.tail linked)))))))

(deftest prepend-test
  (let [linked-prepend (-> linked (dll/prepend 1))]
    (is (= 1
           (:value ((.m linked-prepend) (.head linked-prepend)))))
    (is (= 1
           (:value ((.m linked-prepend) (.tail linked-prepend))))))
  (let [linked-prepend (-> linked
                   (dll/prepend 1)
                   (dll/prepend 2))]
    (is (= true
           (eq-value (.m linked-prepend) [1 2])))
    (is (= 2
           (:value ((.m linked-prepend) (.head linked-prepend)))))
    (is (= 1
           (:value ((.m linked-prepend) (.tail linked-prepend))))))
  (let [linked-prepend (-> linked
                   (dll/prepend 1)
                   (dll/prepend 2)
                   (dll/prepend 1))]
    (is (= true
           (eq-value (.m linked-prepend) [1 1 2])))
    (is (= 1
           (:value ((.m linked-prepend) (.head linked-prepend)))))
    (is (= 1
           (:value ((.m linked-prepend) (.tail linked-prepend)))))))

(deftest append-test
  (let [linked-appended (-> linked (dll/append 1))]
    (is (= 1
           (:value ((.m linked-appended) (.head linked-appended)))))
    (is (= 1
           (:value ((.m linked-appended) (.tail linked-appended))))))
  (let [linked-appended (-> linked
                   (dll/append 1)
                   (dll/append 2))]
    (is (= true
           (eq-value (.m linked-appended) [1 2])))
    (is (= 1
           (:value ((.m linked-appended) (.head linked-appended)))))
    (is (= 2
           (:value ((.m linked-appended) (.tail linked-appended))))))
  (let [linked-appended (-> linked
                   (dll/append 1)
                   (dll/append 2)
                   (dll/append 1))]
    (is (= true
           (eq-value (.m linked-appended) [1 1 2])))
    (is (= 1
           (:value ((.m linked-appended) (.head linked-appended)))))
    (is (= 1
           (:value ((.m linked-appended) (.tail linked-appended)))))))

(deftest delete-test
  (let [linked-appended (-> linked (dll/append 1))]
    (is (= linked-appended
           (-> linked-appended
               (dll/delete 0))))
    (is (= nil
           (-> linked-appended
               (dll/delete 1)
               .m
               seq)))
    (is (= true
           (-> linked-appended
               (dll/append 2)
               (dll/prepend 3)
               (dll/delete 1)
               .m
               (eq-value [2 3]))))
    (is (= true
           (-> linked-appended
               (dll/append 2)
               (dll/prepend 1)
               (dll/prepend 2)
               (dll/delete 2)
               .m
               (eq-value [1 1]))))))

(deftest find*-test
  (is (= nil
         (-> linked
             (dll/find* 1 nil))))
  (is (= nil
         (-> linked
             (dll/append 1)
             (dll/find* 0 nil))))
  (let [linked-appended (-> linked
                   (dll/append 1)
                   (dll/append 2)
                   (dll/append 3))
        finded (-> linked-appended
                   (dll/find* 2 nil))]
    (is (= 2
           (:value finded)))
    (is (= (.head linked-appended)
           (:previous finded)))
    (is (= (.tail linked-appended)
           (:next finded)))))

(deftest delete-tail-test
  (is (= linked
         (-> linked
             dll/delete-tail)))
  (let [linked-appended (-> linked
                            (dll/append 1))]
    (is (= linked
           (-> linked-appended
               dll/delete-tail))))
  (let [linked-appended (-> linked
                            (dll/append 1)
                            (dll/append 2))]
    (is (= true
           (-> linked-appended
               dll/delete-tail
               .m
               (eq-value [1])))))
  (let [linked-appended (-> linked
                            (dll/append 1)
                            (dll/append 2)
                            (dll/append 3))]
    (is (= true
           (-> linked-appended
               dll/delete-tail
               .m
               (eq-value [1 2]))))))

(deftest delete-head-test
  (is (= linked
         (-> linked
             dll/delete-head)))
  (let [linked-appended (-> linked
                            (dll/append 1))]
    (is (= linked
           (-> linked-appended
               dll/delete-head))))
  (let [linked-appended (-> linked
                            (dll/append 1)
                            (dll/append 2))]
    (is (= true
           (-> linked-appended
               dll/delete-head
               .m
               (eq-value [2])))))
  (let [linked-appended (-> linked
                            (dll/append 1)
                            (dll/append 2)
                            (dll/append 3))]
    (is (= true
           (-> linked-appended
               dll/delete-head
               .m
               (eq-value [2 3]))))))

(deftest ->array-test
  (is (= nil
       (-> linked
           dll/->array)))
  (is (= '(1 2 3)
         (-> linked
             (dll/append 1)
             (dll/append 2)
             (dll/append 3)
             dll/->array))))

(deftest ->string-test
  (is (= "[]"
         (-> linked
             (dll/->string nil))))
  (is (= "[1 2 3]"
         (-> linked
             (dll/append 1)
             (dll/append 2)
             (dll/append 3)
             (dll/->string nil)))))
