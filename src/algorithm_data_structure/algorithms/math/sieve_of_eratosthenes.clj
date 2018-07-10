(ns algorithm-data-structure.algorithms.math.sieve-of-eratosthenes)

(defn run [max-number]
  (let [f (fn [number next-number is-prime]
            (if (> next-number max-number)
              is-prime
              (recur number
                     (+ next-number number)
                     (assoc is-prime next-number false))))]
    (loop [number 2
           primes []
           is-prime (assoc (vec (map (constantly true)
                                     (range (inc max-number))))
                           0 false
                           1 false)]
      (if (> number max-number)
        primes
        (let [[primes is-prime] (if (is-prime number)
                                  [(conj primes number)
                                   (f number (* number number) is-prime)]
                                  [primes is-prime])]
          (recur (inc number)
                 primes
                 is-prime))))))

