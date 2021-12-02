(ns bobisageek.aoc2020.day3
  (:require [bobisageek.utils :as u]))

(defn tree-count [tree-map [right down]]
  (let [ ; the lines of input that we'll need to check
        map-lines (map cycle (take-nth down tree-map))
        positions (iterate (partial + right) 0)
        path (map nth map-lines positions)]
    (u/count-if #{\#} path)))

; part 1
#_ (tree-count (u/day-lines 3) [3 1])

; part 2
(def slopes-to-test [[1 1] [3 1] [5 1] [7 1] [1 2]])
#_(->> slopes-to-test
    (map (partial tree-count (u/day-lines 3)))
    (apply *))

