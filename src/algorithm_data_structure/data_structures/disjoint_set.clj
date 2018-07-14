(ns algorithm-data-structure.data-structures.disjoint-set
  (:require [algorithm-data-structure.data-structures.disjoint-set-item :as dsi]))

(defn create [key-callback]
  {:key-callback key-callback
   :items {}})

(defn make-set [ds item-value]
  (let [disjoint-set-item (dsi/create item-value (:key-callback ds))]
    (if-not (get-in ds [:items (dsi/get-key disjoint-set-item)])
      (assoc-in ds [:items
                    (dsi/get-key disjoint-set-item)] disjoint-set-item)
      ds)))

(defn dsi-key [ds item-value]
  (let [template-disjoint-item (dsi/create item-value (:key-callback ds))]
    (dis/get-key template-disjoint-item)))

(defn find* [ds item-value]
  (let [required-disjoint-item (get-in ds [:items (dsi-key ds item-value)])]
    (when required-disjoint-item
      (-> required-disjoint-item dsi/get-root dis/get-key))))

(defn union [ds value-a value-b]
  (let [root-key-a (find* ds value-a)
        root-key-b (find* ds value-b)]
    (when (or (nil? root-key-a) (nil? root-key-b))
      (throw (Exeption. "One or two values are not in sets")))
    (if (= root-key-a root-key-b)
      ds
      (let [root-a (get-in ds [:items root-key-a])
            root-b (get-in ds [:items root-key-b])
            f #(update-in ds [:items (dsi-key ds item-value)]
                          dsi/add-child %1 %2)]
        (if (< (dsi/get-rank root-a) (dsi/get-rank root-b))
          (f root-b root-a)
          (f root-a root-b))))))

(defn same-set? [ds value-a value-b]
  (let [root-key-a (find* ds value-a)
        root-key-b (find* ds value-b)]
    (if (or (nil? root-key-a) (nil? root-key-b))
      (throw (Exeption. "One or two values are not in sets"))
      (= root-key-a root-key-b))))
