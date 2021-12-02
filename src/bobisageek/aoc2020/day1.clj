(ns bobisageek.aoc2020.day1
  (:require [bobisageek.utils :as u]))

(defn parse-input [lines] (map #(Integer/parseInt %) lines))
#_(parse-input (u/day-lines "1min"))

; part 1
(defn look-for-complement [seen cur target]
  (if-let [a (seen cur)]
    (reduced (* a cur))
    (assoc seen (- target cur) cur)))

#_(reduce #(look-for-complement %1 %2 2020) {} (parse-input (u/day-lines "1")))

; part 2
#_(let [inp (vec (parse-input (u/day-lines "1")))
        c (range (count inp))]
    (first
      (for [ind-x c
            ind-y c
            ind-z c
            :when (< ind-x ind-y ind-z)
            :let [x (nth inp ind-x)
                  y (nth inp ind-y)
                  z (nth inp ind-z)]
            :when (= 2020 (+ x y z))]
        (* x y z))))
