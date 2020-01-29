(ns hundreddaysalgo.day3-next-permutation
  (:require [clojure.string :as str]))


(defn next-permutation [input-values]
  (let [input-values (vec input-values)
        last-index (-> input-values count dec)]
    (letfn [(swap [input-values index1 index2]
              (assoc input-values index1 (input-values index2)
                                  index2 (input-values index1)))
            (compare-nth [input-values index1 index2]
              (compare (nth input-values index1)
                       (nth input-values index2)))
            (compute-search-index [input-values last-index]
              (loop [search-index last-index]
                ; find largest non-increasing list member
                (if (and (> search-index 0)
                         (<= 0 (compare-nth input-values (dec search-index) search-index)))
                  (recur (dec search-index))
                  search-index)))
            (compute-swap-index [input-values pivot last-index]
              (loop [swap-index last-index]
                (if (and (>= swap-index 0)
                         (>= 0 (compare-nth input-values swap-index pivot)))
                  (recur (dec swap-index))
                  swap-index)))
            (do-next-permutation [input-values]
              (let [search-index (compute-search-index input-values last-index)]
                (if (<= search-index 0)
                  (reverse input-values)
                  (let [pivot (dec search-index)
                        swap-index (compute-swap-index input-values pivot last-index)
                        permuted-values (swap input-values pivot swap-index)]
                    (loop [permuted-values permuted-values
                           search-index search-index
                           swap-index last-index]
                      (if (< search-index swap-index)
                        (recur (swap permuted-values search-index swap-index)
                               (inc search-index)
                               (dec swap-index))
                        permuted-values))))))]
      (do-next-permutation input-values))))

(comment
  (next-permutation [1 2 3])
  (next-permutation [3 2 1])
  (next-permutation '(1 2 3))
  (next-permutation '(3 2 1))
  (next-permutation (str/split "FADE" #""))
  (next-permutation (str/split "FAED" #""))
  (next-permutation [0, 1, 2, 5, 3, 3, 0])

  )
