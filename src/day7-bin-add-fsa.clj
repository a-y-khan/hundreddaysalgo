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
            "Will coerce numerical input to integer..."
            (Integer/toString (int input) 2))
          (ensure-binary-string [input]
            (zero? (count (dissoc (frequencies input) \1 \0))))
          (input-to-string [input]
            (if (and (string? input) (ensure-binary-string input))
              input
              (integer-to-binary-string input)))
          (get-padding [input1 input2]
            (math/abs (- (count input1) (count input2))))
          (zero-padded-reverse [input pad]
            (concat (reverse input) (repeat pad "0")))
          (inputs-rev-pad [input1 input2 pad]
            (let [input1-split (str/split input1 #"")
                  input2-split (str/split input2 #"")]
              (cond
                (> (count input1-split) (count input2-split))
                [(reverse input1-split) (zero-padded-reverse input2-split pad)]
                (< (count input1-split) (count input2-split))
                [(zero-padded-reverse input1-split pad) (reverse input2-split)]
                )))]
    (let [input1-binary (input-to-string input1)
          input2-binary (input-to-string input2)
          pad (get-padding input1-binary input2-binary)
          inputs-binary-rev-pad (inputs-rev-pad input1-binary input2-binary pad)
          ]
      (println input1-binary input2-binary)
      (println inputs-binary-rev-pad)
      (loop [x (first inputs-binary-rev-pad)
             y (second inputs-binary-rev-pad)
             carry 0
             sum []]
        (if (and (seq x) (seq y))
          (let [x-input (Integer/parseInt (first x))
                y-input (Integer/parseInt (first y))
                carry carry
                state (list x-input y-input carry)]
            (println "loop" state)
            (recur (rest x) (rest y) (:new-carry (get states state)) (conj sum (:output (get states state)))))
          (str/join "" (reverse (conj sum carry)))))

      )
    ))

(comment
  (binary-adder-fsa 3 4)
  (binary-adder-fsa 4 3)
  (binary-adder-fsa 2r11 2r100)
  (binary-adder-fsa "11" "100")
  (binary-adder-fsa "11" "10ab01")
  (def b1 (str/split "1100100100100" #""))
  (def b2 (str/split "100100011000" #""))
  ;; (def b2 (str/split "1100100011000" #""))
  (def b1-rev (reverse b1))
  ; need to pad with 0
  (def b2-rev (concat (reverse b2) (repeat (- (count b1) (count b2)) "0")))
  (println b1-rev)
  (println b2-rev)
  (println (count b1-rev) (count b2-rev))

  (loop [x b1-rev
         y b2-rev
         carry 0
         sum []]
    (if (and (seq x) (seq y))
      (let [x-input (Integer/parseInt (first x))
            y-input (Integer/parseInt (first y))
            carry carry
            state (list x-input y-input carry)]
        (println "loop" state)
        (recur (rest x) (rest y) (:new-carry (get states state)) (conj sum (:output (get states state)))))
      (str/join "" (reverse (conj sum carry)))))

  ;; (def ^:dynamic carry 0)
  ;; (def init-state (list (Integer/parseInt (first b1-rev)) (Integer/parseInt (first b2-rev)) carry))
  ;; (println init-state)
  ;; (println (map #((let [x (Integer/parseInt %1)
                        ;; y (Integer/parseInt %2)]
                    ;; (binding [carry (:new-carry (get states state))]
                      ;; (list x y carry))))
                ;; (rest b1-rev) (rest b2-rev)))
  (println (Integer/toString 2r10 2))
  (println (Integer/toString 8 2))
  (println (Integer/toString 16 2))
  (println (Integer/toString (Integer/parseInt "4") 2))
  (println (Integer/toString (Integer/parseInt "100") 2))
  (println (Integer/parseInt "boo"))
  (frequencies "10abcd01")
  (count (dissoc (frequencies "10abcd01") \1 \0))
  (count (dissoc (frequencies "1001") \1 \0))
  (count (dissoc (frequencies "1234") \1 \0))


  ;; (def ^:dynamic carry 0)
  ;; (map #(let [x (Integer/parseInt %1)
  ;;             y (Integer/parseInt %2)
  ;;             state (list x y carry)
  ;;             sum []]
  ;;         (binding [carry (:new-carry (get states state))]
  ;;           (println "map" state)
  ;;           (conj sum (:output (get states state)))
  ;;           )
  ;;         )
  ;;   b1-rev b2-rev)

  )
