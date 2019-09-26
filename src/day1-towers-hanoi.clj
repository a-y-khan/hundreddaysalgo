(ns hundreddaysalgo.day1-towers-hanoi)

(defn hanoi-simple-helper [height left right middle]
  (if (> height 0)
     (let [next-height (dec height)]
       (hanoi-simple-helper next-height left middle right)
       (println left "=>" right)
       (hanoi-simple-helper next-height middle right left))))

(defn hanoi-simple [height]
  (hanoi-simple-helper height "left" "right" "middle"))

(comment
 (hanoi-simple 1)
 (hanoi-simple 2)
 (hanoi-simple 3))

(defn hanoi
  "Towers of Hanoi with basic recursion and multi-arity"
  ([height] (hanoi height "left" "right" "middle"))
  ([height left right middle]
   (if (> height 0)
     (let [next-height (dec height)]
       (hanoi next-height left middle right)
       (println left "=>" right)
       (hanoi next-height middle right left)))))

(comment
  (hanoi 1 "left" "right" "middle")
  (hanoi 2 "left" "right" "middle")
  (hanoi 3 "left" "right" "middle")
  (hanoi 1)
  (hanoi 2)
  (hanoi 3))
