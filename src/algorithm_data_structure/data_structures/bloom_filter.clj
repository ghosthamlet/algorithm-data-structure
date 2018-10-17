(ns algorithm-data-structure.data-structures.bloom-filter
  "https://github.com/trekhleb/javascript-algorithms/tree/master/src/data-structures/bloom-filter")

(declare hash1 hash2 hash3 get-hash-values)

(defn create
  ([] (create 100))
  ([size]
   {:size size
    :storage (vec (repeat 100 false))}))

(defn- set-value [self index]
  (assoc-in self [:storage index] true))

(defn- get-value [self index]
  (get-in self [:storage index]))

(defn insert [self item]
  (reduce set-value self (get-hash-values self item)))

(defn may-contain [self item]
  (every? #(get-value self %) (get-hash-values self item)))

(defn hash1 [self item]
  (->> self
       :size
       (mod (reduce #(let [hash (->> %2 int (+ (bit-shift-left %1 5) %1))]
                       (Math/abs (bit-and hash hash)))
                    0 item))))

(defn hash2 [self item]
  (->> self
       :size
       (mod (reduce #(->> %2 int (+ (bit-shift-left %1 5) %1)) 5381 item))
       Math/abs))

(defn hash3 [self item]
  (->> self
       :size
       (mod (reduce #(let [hash (->> %2 int (+ (- (bit-shift-left %1 5) %1)))]
                       (bit-and hash hash))
                    0 item))
       Math/abs))

(defn get-hash-values [self item]
  [(hash1 self item)
   (hash2 self item)
   (hash3 self item)])
