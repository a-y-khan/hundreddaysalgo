(ns day4-counting-one-bits)

(defn count-one-bits [value]
  (loop [value value
         count-ones 0]
    (if (zero? value)
      count-ones
      (recur
        (->> value dec (bit-and value))
        (inc count-ones)))))

(comment
  (count-one-bits 3)
  (count-one-bits 2r11)
  (count-one-bits 99)
  (count-one-bits 2r1100011)
  (count-one-bits 2r11001100)
  (count-one-bits 2r11101101)
  (println (-> 99 Integer/toBinaryString Integer/parseInt))

  )
