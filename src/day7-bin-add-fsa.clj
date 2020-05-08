(ns day7-bin-add-fsa
(:require [clojure.string :as str]
          [clojure.spec.alpha :as s]))

(def states {'(0 0 0) {:output 0 :new-carry 0} '(0 1 0) {:output 1 :new-carry 0}
             '(1 0 0) {:output 1 :new-carry 0} '(1 1 0) {:output 0 :new-carry 1}
             '(0 0 1) {:output 1 :new-carry 0} '(0 1 1) {:output 0 :new-carry 1}
             '(1 0 1) {:output 0 :new-carry 1} '(1 1 1) {:output 1 :new-carry 1}})





(comment
  (println states)

  (keys states)
  (get states '(0 0 0))

  (def b1 (str/split "1100100100100" #""))
  (def b2 (str/split "100100011000" #""))
  ;; (def b2 (str/split "1100100011000" #""))
  (def b1-rev (reverse b1))
  ; need to pad with 0
  (def b2-rev (concat (reverse b2) (repeat (- (count b1) (count b2)) "0")))
  (println b1-rev)
  (println b2-rev)
  (println (count b1-rev) (count b2-rev))

  ; TODO: need to try map for this!
  (loop [x b1-rev
         y b2-rev
         carry 0
         sum []]
    (if (and (seq x) (seq y))
      (let [x-input (Integer/parseInt (first x))
            y-input (Integer/parseInt (first y))
            carry carry
            state (list x-input y-input carry)]
        (recur (rest x) (rest y) (:new-carry (get states state)) (conj sum (:output (get states state)))))
      (str/join "" (reverse (conj sum carry)))))

  )
