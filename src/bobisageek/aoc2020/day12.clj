(ns bobisageek.aoc2020.day12
  (:require [bobisageek.utils :as u]
            [clojure.string :as str]))

(defn parse-line [l]
  (mapv #(%1 %2) [first #(Integer/parseInt (str/join %))]
    (split-at 1 l)))
(defn parse [lines]
  (map parse-line lines))

#_(parse (u/day-lines "12min"))

(defn move [[[cur-x cur-y] facing] [instruction amount]]
  (let [turning-right (cycle [\N \E \S \W])]
    (cond
      (= instruction \F) (recur [[cur-x cur-y] facing] [facing amount])
      (= instruction \N) [[cur-x (+ cur-y amount)] facing]
      (= instruction \S) [[cur-x (- cur-y amount)] facing]
      (= instruction \E) [[(+ cur-x amount) cur-y] facing]
      (= instruction \W) [[(- cur-x amount) cur-y] facing]
      (= instruction \R) [[cur-x cur-y] (nth (drop-while #(not= % facing) turning-right) (/ amount 90))]
      (= instruction \L) [[cur-x cur-y] (nth (drop-while #(not= % facing) turning-right) (- 4 (mod (/ amount 90) 4)))])))

(defn part1 [instructions]
  (let [[end-point _] (reduce move [[0 0] \E] instructions)]
    (apply + (map #(Math/abs ^int %) end-point))))

(defn turn [direction [x y :as delta]]
  (let [quadrant (mapv neg? delta)
        changes  {\R [identity -] \L [- identity]}]
    (mapv #(%1 %2) (get changes direction) [y x])))

(defn part2-move [[[cur-x cur-y :as ship] [way-x way-y :as wayp]] [instruction amount]]
  (cond
    (= instruction \F) (let [dist-x     (- way-x cur-x)
                             dist-y     (- way-y cur-y)
                             dx         (* dist-x amount)
                             dy         (* dist-y amount)
                             new-ship-x (+ cur-x dx)
                             new-ship-y (+ cur-y dy)
                             new-wayp-x (+ new-ship-x dist-x)
                             new-wayp-y (+ new-ship-y dist-y)]
                         [[new-ship-x new-ship-y] [new-wayp-x new-wayp-y]])

    (= instruction \N) [ship [way-x (+ way-y amount)]]
    (= instruction \S) [ship [way-x (- way-y amount)]]
    (= instruction \E) [ship [(+ way-x amount) way-y]]
    (= instruction \W) [ship [(- way-x amount) way-y]]
    (#{\R \L} instruction) [ship
                            (mapv + ship
                              (nth (iterate (partial turn instruction) (mapv - wayp ship)) (/ amount 90)))]))


