(ns hundreddaysalgo.day3-next-permutation
  (:require [clojure.string :as str]))

(defn swap [coll index1 index2]
  (assoc coll index1 (coll index2)
              index2 (coll index1)))

(defn compare-nth [coll index1 index2]
  (compare (nth coll index1)
           (nth coll index2)))

(defn next-permutation [input-values]
  (let [input-values (vec input-values)
        last-index (-> input-values count dec)]
    (letfn [(compute-search-index []
              (loop [search-index last-index]
                ; find largest non-increasing list member
                (if (and (< 0 search-index)
                         (<= 0 (compare-nth input-values (dec search-index) search-index)))
                  (recur (dec search-index))
                  search-index)))
            (compute-swap-index [pivot]
              (loop [swap-index last-index]
                (if (and (<= 0 swap-index)
                         (>= 0 (compare-nth input-values swap-index pivot)))
                  (recur (dec swap-index))
                  swap-index)))
            (do-next-permutation []
              (let [search-index (compute-search-index)]
                (if (>= 0 search-index)
                  (reverse input-values)
                  (let [pivot (dec search-index)
                        swap-index (compute-swap-index pivot)
                        permuted-values (swap input-values pivot swap-index)]
                    (loop [permuted-values permuted-values
                           search-index search-index
                           swap-index last-index]
                      (if (< search-index swap-index)
                        (recur (swap permuted-values search-index swap-index)
                               (inc search-index)
                               (dec swap-index))
                        permuted-values))))))]
      (do-next-permutation))))

(comment
  (next-permutation [1 2 3])
  (next-permutation [3 2 1])
  (next-permutation '(1 2 3))
  (next-permutation '(3 2 1))
  (next-permutation (str/split "FADE" #""))
  (next-permutation (str/split "FAED" #""))
  (next-permutation [0, 1, 2, 5, 3, 3, 0])

  )
