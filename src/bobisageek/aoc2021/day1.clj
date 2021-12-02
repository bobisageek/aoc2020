(ns bobisageek.aoc2021.day1
  (:require [bobisageek.utils :as u]))

(defn parse [file]
  (->> file
    (u/day-lines 2021)
    (map #(Integer/parseInt %))))

#_(parse "1min")

(defn part1 [nums]
  (->> nums
    (partition 2 1)
    (u/count-if (partial apply <))))

#_(part1 (parse "1"))

(defn part2 [nums]
  (->> nums
    (partition 3 1)
    (map (partial apply +))
    (part1)))

#_(part2 (parse "1"))
