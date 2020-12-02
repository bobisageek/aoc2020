(ns bobisageek.aoc2020.day1
  (:require [bobisageek.aoc2020.utils :as u]))

(defn parse-input [lines] (map #(Integer/parseInt %) lines))
#_(parse-input (u/day-lines "1min"))

(defn look-for-complement [seen cur target]
  (if-let [a (seen cur)]
    (reduced (* a cur))
    (assoc seen (- target cur) cur)))

; part 1
#_(reduce #(look-for-complement %1 %2 2020) {} (parse-input (u/day-lines "1")))

; part 2
#_(let [inp (vec (parse-input (u/day-lines "1")))
        c (range (count inp))]
    (first
      (for [ind-x c
            ind-y c
            ind-z c
            :let [x (nth inp ind-x)
                  y (nth inp ind-y)
                  z (nth inp ind-z)]
            :when (and (distinct? ind-x ind-y ind-z) (= 2020 (+ x y z)))]
        (* x y z))))

