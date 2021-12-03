(ns bobisageek.aoc2021.day2
  (:require [bobisageek.utils :as u]
            [clojure.string :as str]))

(defn parse [file]
  (->> (u/day-lines 2021 file)
    (map #(str/split % #" "))
    (map (fn [[d n]] [d (Integer/parseInt n)]))))

#_(parse "2min")

(defn solver-thing [reducer]
  (fn [lines]
    (->> (reduce reducer (repeat 0) lines)
         (take 2)
         (reduce *))))

(def part1 (solver-thing 
            (fn [[hor depth] [dir n]]
              (case dir
                "forward" [(+ hor n) depth]
                "down"    [hor (+ depth n)]
                "up"      [hor (- depth n)]))))

(def part2 (solver-thing 
            (fn [[hor depth aim] [dir n]]
              (case dir
                "forward" [(+ hor n) (+ depth (* n aim)) aim]
                "down"    [hor depth (+ aim n)]
                "up"      [hor depth (- aim n)]))))
    
#_(part1 (parse "2"))

#_(part2 (parse "2"))
