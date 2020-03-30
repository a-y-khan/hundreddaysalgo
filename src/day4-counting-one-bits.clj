(ns day4-counting-one-bits
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(defn count-one-bits [value]
  "Count the 1 bits in the binary representation of an integer. Works for integers >= 0."
  (loop [value value
         count-ones 0]
    (if (zero? value)
      count-ones
      (recur
        (->> value dec (bit-and value))
        (inc count-ones)))))
(s/fdef count-one-bits
  :args (s/and (s/cat :value int?)
               #(>= (:value %) 0))
  :ret int?)

(comment
  (stest/instrument `count-one-bits)
  (stest/check `count-one-bits)
  (s/exercise-fn `count-one-bits)

  (s/valid? count-one-bits "a")
  (s/valid? count-one-bits -5)
  (s/valid? count-one-bits 5.0)
  (s/valid? count-one-bits 5)

  (s/explain count-one-bits "a")
  (s/explain count-one-bits -5)
  (s/explain count-one-bits 5.0)
  (s/explain count-one-bits 5)

  (s/conform count-one-bits "a")
  (s/conform count-one-bits -5)
  (s/conform count-one-bits 5.0)
  (s/conform count-one-bits 5)


  (count-one-bits 0)
  (count-one-bits 3)
  (count-one-bits 2r11)
  (count-one-bits 99)
  (count-one-bits 2r1100011)
  (count-one-bits 2r11001100)
  (count-one-bits 2r11101101)

  (println (-> 99 Integer/toBinaryString Integer/parseInt))

  )
