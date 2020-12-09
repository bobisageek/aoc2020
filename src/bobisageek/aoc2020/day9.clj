(ns bobisageek.aoc2020.day9
  (:require [bobisageek.aoc2020.utils :as u]))

(def parse (partial mapv read-string))

#_(parse (u/day-lines "9min"))

(defn all-sums [coll]
  (let [c (range (count coll))]
    (for [a c
          b c
          :when (< a b)]
      (apply + (map (partial nth coll) [a b])))))

(defn part1 [v preamble-length]
  (let [indexes (range preamble-length (count v))
        check   #(some #{(nth v %)}
                   (all-sums (subvec v (- % preamble-length) %)))]
    (v (first (remove check indexes)))))

#_(part1 (parse (u/day-lines "9")) 25)

(defn aggregate [v]
  (+ (apply min v) (apply max v)))

(defn part2 [v preamble-length]
  (let [target (part1 v preamble-length)]
    (loop [acc   0
           start 0
           end   0]
      (cond
        (= target acc) (aggregate (subvec v start end))
        (> target acc) (recur (+ acc (nth v end)) start (inc end))
        (< target acc) (recur (- acc (nth v start)) (inc start) end)))))

#_(part2 (parse (u/day-lines "9")) 25)
