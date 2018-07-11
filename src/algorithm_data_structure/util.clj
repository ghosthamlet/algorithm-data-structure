(ns algorithm-data-structure.util)

(defn but-nth [xs pos]
  (vec (case pos
         :first (next xs)
         :last (butlast xs)
         (concat (subvec xs 0 pos) (subvec xs (inc pos))))))

(defn move [xs from to]
  (assoc (but-nth xs from)
         to (case from
              :first (first xs)
              :last (last xs)
              (nth xs from))))

(defn default-compare [a b]
  (if (= a b)
    0
    (if (< a b) -1 1)))

(defn- compare-action [f action a b]
  (case action
    :equal (zero? (f a b))
    :less-then (neg? (f a b))
    (throw (Exception. "no implemention"))))

(defn compare-value
  ([action]
   (partial compare-action compare-value action))
  ([action a b]
   (compare-action compare-value action a b))
  ([a b]
   (default-compare a b)))

(defmacro ->m [& xs]
  (reduce #(assc %1 (keyword %2) %2) {} xs))

(defn includes [xs x]
  (some #(= x %) xs))