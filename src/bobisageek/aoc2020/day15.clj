(ns bobisageek.aoc2020.day15)

(def puzzle-input [11 18 0 20 1 7 16])

(defn part1 [input-seq target-count]
  (loop [history (into (list) (map vector input-seq (iterate inc 1)))]
    (let [[last-num last-count] (first history)
          first-two    (take 2 (filter #(= last-num (first %)) history))
          next-num     (if (= 2 (count first-two))
                         (apply - (map second first-two))
                         0)
          next-counter (inc last-count)]
      (if (= target-count next-counter)
        next-num
        (recur (conj history [next-num next-counter]))))))

#_ (part1 puzzle-input 2020)


(defn part2 [input-seq target-count]
  (loop [history    (reduce (fn [m [k v]]
                              (update m k conj v)) {} (map vector input-seq (iterate inc 1)))
         last-num   (last input-seq)
         last-count (count input-seq)]
    (let [first-two    (take 2 (history last-num))
          next-num     (if (= 2 (count first-two))
                         (apply - first-two)
                         0)
          next-counter (inc last-count)]
      (if (= target-count next-counter)
        next-num
        (do
          (when (zero? (mod next-counter 10000)) (println next-counter))
          (recur (update history next-num conj next-counter)
            next-num
            next-counter))))))

#_ (part2 puzzle-input 30000000)
