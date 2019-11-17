(ns hundreddaysalgo.day2-matrix-chain-mult)

;; n = len(chain) - 1
;; # for simplicity, allocating nxn lists of lists
;; # to store subproblem results
;; cost_lists = [[0 for x in range(n)] for x in range(n)] # m
;; index_lists = [[0 for x in range(n)] for x in range(n)] # s
;; for l in range(1, n):
;;   for i in range(0, n-l):
;;     j = i + l
;;     cost_lists[i][j] = float('inf')
;;     for k in range(i, j):
;;       q = cost_lists[i][k] + cost_lists[k+1][j] + chain[i] * chain[k+1] * chain[j+1]
;;       if q < cost_lists[i][j]:
;;         cost_lists[i][j] = q
;;         index_lists[i][j] = k
;; min_cost_indices = (0, n-1)
;; return cost_lists[min_cost_indices[0]][min_cost_indices[1]], min_cost_indices, cost_lists, index_lists

;; https://stackoverflow.com/questions/4112217/idiomatic-clojure-for-solving-dynamic-programming-algorithm
(defn matrix-chain-mult [chain]
  (letfn [(mem-table-size [chain]
            (dec (count chain)))
          (table [n]
            (vec (repeat n (vec (repeat n 0)))))
          (table-lookup [table indexes]
            (nth (nth table (first indexes)) (second indexes)))
          (chain-lookup [chain index]
            (nth chain index))
          (update-table? [cost-table new-cost indexes]
            (< new-cost (table-lookup cost-table indexes)))
          (update-cost [cost-table new-cost indexes update-table]
            (if update-table
              (assoc-in cost-table indexes new-cost)
              cost-table))
          (update-index [index-table new-cost-index indexes update-table]
            (if update-table
              (assoc-in index-table indexes new-cost-index)
              index-table))

          ;; TODO: try trampoline!!!
          (compute-costs [tables chain i j]
            (loop [tables tables
                   k i
                   ;; update-table false
                   ]
              (if (= k j)
                tables
                (recur (let [new-cost (+ (table-lookup (:cost tables) [i k])
                                         (table-lookup (:cost tables) [(inc k) j])
                                         (chain-lookup chain i)
                                         (chain-lookup chain (inc k))
                                         (chain-lookup chain (inc j)))
                             do-update? (update-table? (:cost tables) new-cost [i j])]
                         (assoc tables
                                :cost (update-cost (:cost tables) new-cost [i j] do-update?)
                                :index (update-index (:index tables) k [i j] do-update?)))
                       (inc k)))))

          (init-costs [cost-table l n]
            (let [end-index (- n l)]
              (loop [cost-table cost-table
                     i 0
                     j l]
                (if (= i end-index)
                  cost-table
                  (recur (assoc-in cost-table [i j] ##Inf) (inc i) (+ (inc i) l))))))

          (run-compute-costs [tables chain l n]
            (let [end-index (- n l)]
              (loop [tables tables
                     i 0
                     j l]
                (if (= i end-index)
                  tables
                  (recur (compute-costs tables chain i j) (inc i) (+ (inc i) l))))))

          (do-matrix-chain-mult [chain]
            (let [n (mem-table-size chain)
                  min-cost-indexes [0 (- n 1)]
                  index-table (table n)
                  cost-table (loop [cost-table (table n)
                                    l 1]
                               (if (= l n)
                                 cost-table
                                 (recur (init-costs cost-table l n) (inc l))))]
              (loop [tables {:cost cost-table :index index-table}
                     l 1]
                (if (= l n)
                  {:min-cost (table-lookup (:cost tables) min-cost-indexes)
                   :min-cost-indexes min-cost-indexes
                   :cost (:cost tables)
                   :index (:index tables)}
                  (recur (run-compute-costs tables chain l n) (inc l))))))
          ]
    ;; (println (mem-table-size chain))
    ;; (println (table (mem-table-size chain)))
    (do-matrix-chain-mult chain)
    )
  )

(comment
  ;; min # multiplications = 0
  (def result (matrix-chain-mult [2 2]))
  (println (:min-cost result))
  ;; min # of multiplications = 18
  (def result (matrix-chain-mult [1 2 3 4]))
  (println (:min-cost result))
  ;; (reduce * (map * [1 2 3] [1 2 3]))

  (loop [i 0
         m {:a 0 :b 1}]
    (if (> i 9)
      m
      (recur (inc i) (assoc m :a (inc (:a m))))))

  (defn test-loop [n]
    (loop [i 0
           m {:a 0 :b n}]
      (if (> i 9)
        {:x (:a m) :y (:b m) :z 44}
        (recur (inc i) (assoc m :a i)))))
  (test-loop 999)

  (def m {:a [[0 1 2] [3 4 5]] :b [[6 7 8] [9 10 11]]})
  ;; (def m (assoc-in m [:a 0 0] 9))
  ;; (assoc-in m [:a 1 2] 99 [:b 1 2] 999)
  (assoc-in m [:a 1 2] 99)
  (println m)
  ;; (doseq [x (range 1 6)] (println x))
  (defn test-table-lookup [table indexes]
    (println (first indexes) (second indexes))
    (nth (nth table (first indexes)) (second indexes)))
  (test-table-lookup (assoc-in [[0 0 0] [0 0 0]] [0 0] 1) [1 0])
  (def p {:a 10 :b 20})
  (println p)
  (println (assoc p :a 40 :c 30))
  (let [a (+ 1 1)
        b (+ a 1)]
    (println a b))
  (println ##Inf)
  (println (< ##Inf 0))
  (loop [i 0
         j (- i 1)
         k (- j 1)
         t nil]
    (if (= i 4)
      (println i j k)
      (recur (inc i) (- i 1) (- j 1) (println i j k))))
  (loop [i 0
         j (+ i 1)]
    (when (< i 5)
      (println i j)
      (recur (inc i) (+ (inc i) 1))))

  (defn foo [x]
    (if (= x 10)
      (println "done:" x)
      #(foo (do (println "x trampoline:" x) (inc x)))))
  (trampoline foo 0)

  (loop [i 0
         j (+ i 2)]
    (if (< i 10)
      (do
        (println "do:" i j)
        (recur (inc i) (inc j)))
      i))

  )
