(ns bobisageek.aoc2021.day5
  (:require [bobisageek.utils :as u]))

(defn parse [file]
  (let [raw (u/rgx-extract-pieces #"(\d+),(\d+) -> (\d+),(\d+)" (u/day-lines 2021 file))
        nums (map #(map u/parse-long %) raw)
        partitioned (map #(mapv vec (partition 2 %)) nums)]
    partitioned))

#_(parse "5min")

(defn range' [start end]
  (cond
    (= start end) (repeat start)
    (< start end) (range start (inc end))
    :else         (range start (dec end) -1)))

(defn points-of 
  "Return the line from the points passed."
  [[[x1 y1 :as p1] [x2 y2 :as p2]]]
  (let [[xs ys] (cond
                  (= p1 p2) [[x1] [y1]]
                  :else     [(range' x1 x2) (range' y1 y2)])]
    (map vector xs ys)))
    
(defn part1-filter [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2)))

(defn solver [filter-fn coords]
  (let [filtered-coords (filter filter-fn coords)
        all-points (mapcat points-of filtered-coords)
        grouped (frequencies all-points)]
    (u/count-if (fn [[_ v]] (< 1 v)) grouped)))

(def part1 (partial solver part1-filter))
(def part2 (partial solver any?))

#_(part1 (parse "5"))
#_(part2 (parse "5"))

