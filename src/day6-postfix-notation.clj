(ns day6-postfix-notation
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(def ops {"+" #(+ %1 %2)
          "-" #(- %1 %2)
          "*" #(* %1 %2)
          "/" #(quot %1 %2)
          "^" #(math/expt %1 %2)})

(defn postfix-notation
  [expression]
  (letfn [(do-op [x op-list]
            (let [args (take-last 2 op-list)
                  op-list (vec (drop-last 2 op-list))]
              (conj op-list ((get ops x) (first args) (second args)))))
          (build-op [x op-list]
            (if (contains? ops x)
              (do-op x op-list)
              (conj op-list (Float/parseFloat x))))
          (do-postfix-notation [expression]
            (loop [op-list []
                   expression-tokens (str/split expression #" ")]
              (if (empty? expression-tokens)
                (peek op-list)
                (recur (build-op (first expression-tokens) op-list)
                       (rest expression-tokens)))))]
    (do-postfix-notation expression)))
(s/fdef postfix-notation
  :args (s/cat :expression string?)
  :ret number?)

(comment
  (s/valid? postfix-notation "1 2 +")
  (s/conform postfix-notation "1 2 +")

  (postfix-notation "1 2 +")
  (postfix-notation "1 2 3 4 5 + + + +")
  (postfix-notation "1 2 + 4 3 - + 10 5 / *")
  (postfix-notation "1 2 * 6 2 / + 9 7 - ^")

  )

