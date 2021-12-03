(ns bobisageek.aoc2021.day1
  (:require [bobisageek.utils :as u]))

(defn parse [file]
  (->> file
    (u/day-lines 2021)
    (map #(Integer/parseInt %))))

#_(parse "1min")

(defn count-chunked-sum-increases [chunk-size nums]
  (->> nums
    (partition chunk-size 1)
    (map (partial apply +))
    (partition 2 1)
    (u/count-if (partial apply <))))

(def part1 (partial count-chunked-sum-increases 1))

#_(part1 (parse "1"))

(def part2 (partial count-chunked-sum-increases 3))

#_(part2 (parse "1"))
