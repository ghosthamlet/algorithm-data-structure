(ns algorithm-data-structure.comparator)

(defn compare-default [_ a b]
  (cond
    (= a b) 0
    (< a b) -1
    :else 1))

(defmulti compare-value (fn [self _ _] (type self)))

(defmethod compare-value :default [self a b]
  (compare-default self a b))

(defmacro defcompare [name expr]
  (list 'defn name '[self a b & [f]]
        (list 'let '[f (or f compare-value)]
              expr)))

(defcompare equal (zero? (f self a b)))

(defcompare less-then (neg? (f self a b)))

(defcompare greater-than (pos? (f self a b)))

(defcompare less-then-or-equal
  (or (less-then self a b) (equal self a b)))

(defcompare greater-than-or-equal
  (or (greater-than self a b) (equal self a b)))
