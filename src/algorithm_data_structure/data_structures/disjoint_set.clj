(ns algorithm-data-structure.data-structures.disjoint-set
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/disjoint-set"
  (:require [algorithm-data-structure.data-structures.disjoint-set-item :as dsi]))

(defn create
  ([] (create nil))
  ([key-callback]
   {:key-callback key-callback
    :items {}}))

(defn make-set [self item-value]
  (let [disjoint-set-item (dsi/create item-value (:key-callback self))]
    (if-not (get-in self [:items (dsi/get-key disjoint-set-item)])
      (assoc-in self [:items
                    (dsi/get-key disjoint-set-item)] disjoint-set-item)
      self)))

(defn- dsi-key [self item-value]
  (let [template-disjoint-item (dsi/create item-value (:key-callback self))]
    (dsi/get-key template-disjoint-item)))

(defn find* [self item-value]
  (when-let [required-disjoint-item (get-in self [:items (dsi-key self item-value)])]
    (-> required-disjoint-item dsi/get-root dsi/get-key)))

(defn union [self value-a value-b]
  (let [root-key-a (find* self value-a)
        root-key-b (find* self value-b)]
    (when (or (nil? root-key-a) (nil? root-key-b))
      (throw (Exception. "One or two values are not in sets")))
    (if (= root-key-a root-key-b)
      self
      (let [root-a (get-in self [:items root-key-a])
            root-b (get-in self [:items root-key-b])
            f #(update-in self [:items (dsi-key self (:value %1))]
                          dsi/add-child %2)]
        (if (< (dsi/get-rank root-a) (dsi/get-rank root-b))
          (f root-b root-a)
          (f root-a root-b))))))

(defn same-set? [self value-a value-b]
  (let [root-key-a (find* self value-a)
        root-key-b (find* self value-b)]
    (if (or (nil? root-key-a) (nil? root-key-b))
      (throw (Exception. "One or two values are not in sets"))
      (= root-key-a root-key-b))))
