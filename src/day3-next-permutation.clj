(ns hundreddaysalgo.day3-next-permutation
  (:require [clojure.string :as str]))


(defn get-longest-nonincreasing-item-index [input-values]
  (loop [search-index (-> input-values count dec)]
    (if (and (> search-index 0)
             (<= 0 (compare
                    (nth input-values (dec search-index))
                    (nth input-values search-index))))
      (recur (dec search-index))
      search-index)))

(defn get-swap-item-index [input-values pivot]
  (loop [swap-index (-> input-values count dec)]
    (if (and (>= swap-index 0)
             (>= 0 (compare (nth input-values swap-index)
                            (nth input-values pivot))))
      (recur (dec swap-index))
      swap-index)))

(defn swap [input-values index1 index2]
  (assoc input-values index1 (input-values index2)
                      index2 (input-values index1)))

(defn next-permutation [input-values]
  (let [input-values (vec input-values)
        search-index (get-longest-nonincreasing-item-index input-values)]
    (if (<= search-index 0)
      (reverse input-values)
      (let [pivot (dec search-index)
            swap-index (get-swap-item-index input-values pivot)
            permuted-values (swap input-values pivot, swap-index)]
        (loop [permuted-values permuted-values
               search-index search-index
               swap-index (-> input-values count dec)]
          (if (< search-index swap-index)
            (recur (swap permuted-values search-index swap-index)
                   (inc search-index)
                   (dec swap-index))
            permuted-values))))))

(comment
  (next-permutation [1 2 3])
  (next-permutation [3 2 1])
  (next-permutation '(1 2 3))
  (next-permutation '(3 2 1))
  (next-permutation (str/split "FADE" #""))
  (next-permutation (str/split "FAED" #""))
  (next-permutation [0, 1, 2, 5, 3, 3, 0])

  )
