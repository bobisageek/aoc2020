(ns bobisageek.aoc2020.day5
  (:require [bobisageek.utils :as u]))

(def seat-num
  (comp #(Integer/parseInt % 2)
    (partial apply str)
    #(map {\B 1 \R 1 \F 0 \L 0} %)))

(defn part1 [input]
  (->> (into (sorted-set-by >) (map seat-num input))
    first))

#_(part1 (u/day-lines "5"))

(defn part2 [input]
  (let [seat-nums          (into (sorted-set) (map seat-num input))
        min                (first seat-nums)
        possible-seat-nums (iterate inc min)]
    (ffirst (filter (partial apply not=) (map vector possible-seat-nums seat-nums)))))

#_(part2 (u/day-lines "5"))
