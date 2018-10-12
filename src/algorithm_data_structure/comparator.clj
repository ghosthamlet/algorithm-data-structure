(ns algorithm-data-structure.comparator)

(defn compare-default [_ a b]
  (if (and (instance? Number a) (instance? Number b))
    (cond
      (= a b) 0
      (< a b) -1
      :else 1)
    (do
      (prn "Warning: Compare value is not number, using default comparator,
            this can't used for other compare except equal.")
      (cond
       (= a b) 0
       :else 1))))

(defmulti compare-value (fn [self a _]
                          [(type self) (type a)]))

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
