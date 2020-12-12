(ns bobisageek.aoc2020.day11
  (:require [bobisageek.aoc2020.utils :as u]
            [clojure.set :as set]))

(def surroundings
  (memoize (fn [[col line]]
             (set (for [cols  [(dec col) col (inc col)]
                        lines [(dec line) line (inc line)]
                        :when (not (and (= cols col) (= lines line)))]
                    [cols lines])))))

#_(surroundings [3 5])

; region part 1
(defn parse-line [line-num s]
  (reduce-kv (fn [m i c] (update m c (fnil conj #{}) [i line-num]))
    {} (vec s)))
(defn parse [input-lines]
  (apply merge-with set/union (map parse-line (range) input-lines)))

(defn empty-check [empties occupieds]
  (letfn [(c [m p]
            (update m (if (empty? (set/intersection (surroundings p) occupieds))
                        \#
                        \L) (fnil conj #{}) p))]
    (reduce c {} empties)))

(defn occupied-check [occupieds]
  (letfn [(c [m p]
            (update m (if (<= 4 (count (set/intersection (surroundings p) occupieds)))
                        \L
                        \#) (fnil conj #{}) p))]
    (reduce c {} occupieds)))

(defn one-generation [starting-state]
  (let [occupieds         (starting-state \#)
        advance-empties   (empty-check (starting-state \L) occupieds)
        advance-occupieds (occupied-check occupieds)]
    (merge-with set/union advance-empties advance-occupieds)))

(defn part1 [starting-state]
  (let [new-state (one-generation starting-state)]
    (if (= starting-state new-state)
      (count (new-state \#))
      (recur new-state))))

#_(part1 (parse (u/day-lines "11min")))

; endregion part 1

(defn part2-parse1 [line-num line]
  (reduce (fn [m [i c]] (if (#{\L \#} c)
                          (assoc m [i line-num] c)
                          m)) {} (map vector (range) line)))

(defn part2-parse [input-lines]
  (reduce merge (map part2-parse1 (range) input-lines)))

(def directions (for [x [-1 0 1]
                      y [-1 0 1]
                      :when (not= [x y] [0 0])]
                  [x y]))
(defn visible-seats [seat-pos seats w h]
  (let [find (map #(first (filter seats (take-while (fn [[x y]]
                                                      (and (<= 0 x w) (<= 0 y h)))
                                          (drop 1 (iterate (partial mapv + %1) seat-pos))))) directions)]
    (remove nil? find)))

(defn check-point [p to-check seats]
  (let [this-seat    (seats p)
        occupied-num (u/count-if #{\#} (map seats to-check))]
    (cond
      (and (= this-seat \L) (zero? occupied-num)) [p \#]
      (= this-seat \L) [p \L]
      (and (= this-seat \#) (<= 5 occupied-num)) [p \L]
      :else [p \#])))

(defn part2 [initial-state]
  (let [all-seats        (set (keys initial-state))
        max-width        (apply max (map first all-seats))
        max-height       (apply max (map second all-seats))
        visible-seat-map (into {} (map #(vector % (visible-seats % all-seats max-width max-height)) all-seats))]
    (loop [s initial-state]
      (let [new-state     (into {} (map #(check-point % (visible-seat-map %) s) all-seats))
            initial-count (u/count-if #(= \# %) (vals s))
            new-count     (u/count-if #(= \# %) (vals new-state))]
        (if (= initial-count new-count)
          new-count
          (recur new-state))))))

#_(time (part2 (part2-parse (u/day-lines "11"))))
