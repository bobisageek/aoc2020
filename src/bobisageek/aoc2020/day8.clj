(ns bobisageek.aoc2020.day8
  (:require [bobisageek.utils :as u]
            [clojure.string :as str]))

(defn parse-line [l]
  (mapv #(%1 %2) [identity #(Integer/parseInt %)] (str/split l #" +")))

(defn parse-input [lines]
  (zipmap (range) (map parse-line lines)))

#_(parse-input (u/day-lines "8min"))

(defn part1-acc [program]
  (loop [pos  0
         acc  0
         seen #{}]
    (let [[op num] (program pos)]
      (cond
        (nil? (program pos)) [:halt acc]
        (seen pos) [:loop acc]
        (= op "nop") (recur (inc pos) acc (conj seen pos))
        (= op "jmp") (recur (+ pos num) acc (conj seen pos))
        (= op "acc") (recur (inc pos) (+ acc num) (conj seen pos))))))
; region part 1
#_(part1-acc (parse-input (u/day-lines "8")))
; endregion part 1

(defn part2 [program]
  (let [subs {"jmp" "nop"
              "nop" "jmp"}]
    (loop [pos 0]
      (let [[ins num] (program pos)]
        (if-let [new-ins (subs ins)]
          (let [[ended-at acc] (part1-acc (assoc program pos [new-ins num]))]
            (if (= ended-at :halt)
              acc
              (recur (inc pos))))
          (recur (inc pos)))))))

#_ (part2 (parse-input (u/day-lines "8")))
