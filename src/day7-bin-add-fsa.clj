(ns day7-bin-add-fsa
(:require [clojure.string :as str]
          [clojure.spec.alpha :as s]
          [clojure.math.numeric-tower :as math]))

(def states {'(0 0 0) {:output 0 :new-carry 0} '(0 1 0) {:output 1 :new-carry 0}
             '(1 0 0) {:output 1 :new-carry 0} '(1 1 0) {:output 0 :new-carry 1}
             '(0 0 1) {:output 1 :new-carry 0} '(0 1 1) {:output 0 :new-carry 1}
             '(1 0 1) {:output 0 :new-carry 1} '(1 1 1) {:output 1 :new-carry 1}})


(defn binary-adder-fsa [input1 input2]
  (letfn [(integer-to-binary-string [input]
            "Will coerce numerical input to integer."
            (Integer/toString (int input) 2))
          (ensure-binary-string [input]
            (zero? (count (dissoc (frequencies input) \1 \0))))
          (input-to-string [input]
            (if (and (string? input) (ensure-binary-string input))
              input
              (integer-to-binary-string input)))
          (get-pad [input1 input2]
            (math/abs (- (count input1) (count input2))))
          (zero-padded-reverse [input pad]
            (concat (reverse input) (repeat pad "0")))
          (padded-reverse-inputs [input1 input2 pad]
            (let [input1-split (str/split input1 #"")
                  input2-split (str/split input2 #"")]
              (cond
                (> (count input1-split) (count input2-split))
                {:i1 (reverse input1-split) :i2 (zero-padded-reverse input2-split pad)}
                (< (count input1-split) (count input2-split))
                {:i1 (zero-padded-reverse input1-split pad) :i2 (reverse input2-split)})))]
    (let [input1-binary (input-to-string input1)
          input2-binary (input-to-string input2)
          pad (get-pad input1-binary input2-binary)
          inputs-binary-rev-pad (padded-reverse-inputs input1-binary input2-binary pad)]
      (loop [x (:i1 inputs-binary-rev-pad)
             y (:i2 inputs-binary-rev-pad)
             carry 0
             sum []]
        (if (and (seq x) (seq y))
          (let [x-input (Integer/parseInt (first x))
                y-input (Integer/parseInt (first y))
                carry carry
                state (list x-input y-input carry)]
            (recur (rest x) (rest y) (:new-carry (get states state)) (conj sum (:output (get states state)))))
          (str/join "" (reverse (conj sum carry))))))))

(comment
  (= 7 (Integer/parseInt (binary-adder-fsa 3 4) 2))
  (= 7 (Integer/parseInt (binary-adder-fsa 4 3) 2))
  (= 2r111 (Integer/parseInt (binary-adder-fsa 2r11 2r100) 2))
  (= 2r111 (Integer/parseInt (binary-adder-fsa "11" "100") 2))
  (= "10001000111100" (binary-adder-fsa "1100100100100" "100100011000"))
  (= 8764 (Integer/parseInt (binary-adder-fsa "1100100100100" "100100011000") 2))
  ;; (binary-adder-fsa "11" "10ab01") ; shoudl fail
  )
