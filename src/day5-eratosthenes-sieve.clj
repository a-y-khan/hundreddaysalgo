(ns day5-eratosthenes-sieve
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

;; https://www.geeksforgeeks.org/sieve-eratosthenes-0n-time-complexity/
(defn eratosthenes-sieve
  "Sieve of Eratosthenes: find all prime numbers less than or equal to integer n."
  [n]
  (letfn [(conj-prime [primes value]
            (conj (:primes primes) value))
          (nth-is-primes [primes index]
            (nth (:is-primes primes) index))
          (nth-smallest-prime-factor [primes index]
            (nth (:smallest-prime-factors primes) index))
          (nth-primes [primes index]
            (nth (:primes primes) index))
          (count-primes [primes]
            (count (:primes primes)))
          (set-primes [primes i]
            (if (nth-is-primes primes i)
              (-> primes
                  (assoc :primes (conj-prime primes i))
                  (assoc-in [:smallest-prime-factors i] i))
              primes))
          (cleanup-primes [primes i j]
            (-> primes
                ;; remove multiples of i * jth prime, which are (obviously) not prime
                (assoc-in [:is-primes (* i (nth-primes primes j))] false)
                ;; put smallest prime factor of i * jth prime
                (assoc-in [:smallest-prime-factors (* i (nth-primes primes j))] (nth-primes primes j))))
          (remove-prime-multiples [primes i n]
            (loop [primes primes
                   j 0]
              (if (and (< j (count-primes primes))
                       (< (* i (nth-primes primes j)) n)
                       (<= (nth-primes primes j) (nth-smallest-prime-factor primes i)))
                (recur (cleanup-primes primes i j) (inc j))
                primes)))
          (compute-sieve [n]
            (loop
                [primes {:is-primes (-> (vec (repeat n true))
                                        (assoc 0 false)
                                        (assoc 1 false))
                         :smallest-prime-factors (vec (repeat n nil))
                         :primes []}
                 i 2]
              (if (< i n)
                (let [primes (set-primes primes i)]
                  (recur (remove-prime-multiples primes i n) (inc i)))
                (:primes primes))))]
    (compute-sieve n)))
(s/fdef eratosthenes-sieve
  :args (s/and (s/cat :n int?)
               #(>= (:n %) 0))
  :ret (s/coll-of int?))

(comment
  (stest/instrument `eratosthenes-sieve)
  (stest/check `eratosthenes-sieve)
  (s/exercise-fn `eratosthenes-sieve)

  (s/valid? eratosthenes-sieve 3)
  (s/conform eratosthenes-sieve 3)
  (s/valid? eratosthenes-sieve 3.)
  (s/conform eratosthenes-sieve 3.)
  (s/valid? eratosthenes-sieve -1)
  (s/conform eratosthenes-sieve -1)

  (eratosthenes-sieve 3)
  (eratosthenes-sieve 9)
  (eratosthenes-sieve 13)
  (eratosthenes-sieve 15)
  (eratosthenes-sieve 30)

  (loop [i 0]
    (when (< i 5)
      (println i)
      (recur (inc i))))
  (loop [my-coll []
         i 0]
    (if (< i 5)
      (recur (conj my-coll i) (inc i))
      my-coll))
  (if-let [x (> 5 1) y 10]
    (println x y)
    )
  (defn t [n]
    (letfn [(u [n] (* n 2))
            (v [n] (* n n))]
      (println (u n) (v n)))
    )
  (t 2)
  (t 3)
  )
