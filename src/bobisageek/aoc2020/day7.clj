(ns bobisageek.aoc2020.day7
  (:require [bobisageek.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

#_(u/day-lines "7min")
(defn parse-line [l]
  (let [[color holds] (first (u/rgx-extract-pieces #"(.*) bags contain (.*)." [l]))
        color-and-num (filter seq (map vec (u/rgx-extract-pieces #"(\d+) (.*) bags?" (str/split holds #","))))]
    [color (into {} (set/map-invert color-and-num))]))

(def parse-input #(into {} (map parse-line %)))

; region part 1
(defn part1-r [m [k vs]]
  (let [pairs (map vector (keys vs) (repeat k))]
    (reduce #(update %1 (first %2) (fnil conj #{}) (second %2)) m pairs)))

(defn part1-find-paths [m k]
  (loop [can-contain (m k)]
    (let [nc (apply set/union can-contain (map m can-contain))]
      (if (= can-contain nc)
        nc
        (recur nc)))))
; endregion part 1

#_(let [p (parse-input (u/day-lines "7"))
        r (reduce part1-r {} p)]
    (count (part1-find-paths r "shiny gold")))

(defn part2-contents [m color]
  (if (empty? (m color))
    0
    (reduce + (map #(* (Integer/parseInt (second %))
                      (inc (part2-contents m (first %))))
                (m color)))))
#_(let [p (parse-input (u/day-lines "7"))]
    (part2-contents p "shiny gold"))
