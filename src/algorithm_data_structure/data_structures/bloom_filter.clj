(ns algorithm-data-structure.data-structures.bloom-filter)

(declare hash1 hash2 hash3 get-hash-values)

(defn create
  ([] (create 100))
  ([size]
   {:size size
    :storage (vec (map (constantly false) (range size)))}))

(defn- set-value [bf index]
  (assoc-in bf [:storage index] true))

(defn- get-value [bf index]
  (get-in bf [:storage index]))

(defn insert [bf item]
  (reduce set-value bf (get-hash-values bf item)))

(defn may-contain [bf item]
  (every? #(get-value bf %) (get-hash-values bf item)))

(defn hash1 [bf item]
  (->> bf
       :size
       (mod (reduce #(let [hash (->> %2 int (+ (bit-shift-left %1 5) %1))]
                       (Math/abs (bit-and hash hash)))
                    0 item))))

(defn hash2 [bf item]
  (->> bf
       :size
       (mod (reduce #(->> %2 int (+ (bit-shift-left %1 5) %1)) 5381 item))
       Math/abs))

(defn hash3 [bf item]
  (->> bf
       :size
       (mod (reduce #(let [hash (->> %2 int (+ (- (bit-shift-left %1 5) %1)))]
                       (bit-and hash hash))
                    0 item))
       Math/abs))

(defn get-hash-values [bf item]
  [(hash1 bf item)
   (hash2 bf item)
   (hash3 bf item)])
