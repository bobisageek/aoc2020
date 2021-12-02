(ns bobisageek.aoc2020.day10
  (:require [bobisageek.utils :as u]))

#_ (as-> (map read-string (u/day-lines "10")) $
     (conj $ 0)
     (sort $)
     (map - (drop 1 $) $)
     (frequencies $)
     (update $ 3 inc))

#_ (as-> (map read-string (u/day-lines "10")) $
     (conj $ 0)
     (conj $ (+ 3 (apply max $)))
     (sort $)
     (map - (drop 1 $) $)
     (partition-by identity $)
     (take-nth 2 $)
     (map count $)
     (map {4 7 3 4 2 2 1 1} $)
     (apply * 1N $))

