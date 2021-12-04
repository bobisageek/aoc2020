(ns bobisageek.aoc2021.day3
  (:require [bobisageek.utils :as u]
            [clojure.string :as str]))


(defn sort-freqs
  "Sort elements of coll by occurences ascending and 
  identity ascending. Implies that elements of coll must be comparable."
  [coll]
  (->> (frequencies coll)
       (sort-by (juxt val key))
       (map first)))

(defn bin-*
  "multiply zero or more binary numbers, represented as strings"
  [& bins]
  (->> bins
       (map #(Integer/parseInt % 2))
       (apply *)))

(defn part1 [lines]
  (let [chars (apply map vector lines)
        min-maxes (map sort-freqs chars)
        gamma-chars (map second min-maxes)
        epsilon-chars (map first min-maxes)]
    (apply bin-* (map str/join [gamma-chars epsilon-chars]))))
    
#_(part1 (u/day-lines 2021 "3"))
  ;; => 2498354

(defn popularities [selector]
  (fn [coll pos]
    (if (= 1 (count coll))
        (first coll)
        (let [chars (map #(nth % pos) coll)
              chosen (selector (sort-freqs chars))]
           (recur (filter #(= chosen (nth % pos)) coll) (inc pos))))))

(defn part2 [lines]
  (let [fns (map popularities [second first])
        [oxy-gen-rtng co2-scrubber-rtng] (map #(% lines 0) fns)]
    (bin-* oxy-gen-rtng co2-scrubber-rtng)))

#_(part2 (u/day-lines 2021 "3"))
  ;; => 3277956


