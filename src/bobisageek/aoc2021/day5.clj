(ns bobisageek.aoc2021.day5
  (:require [bobisageek.utils :as u]))

(defn parse [file]
  (u/rgx-extract-pieces #"(\d+),(\d+) -> (\d+),(\d+)" (u/day-lines 2021 file)))

#_(parse "5min")

