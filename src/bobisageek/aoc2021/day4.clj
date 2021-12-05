(ns bobisageek.aoc2021.day4
  (:require [bobisageek.utils :as u]
            [clojure.set :as set]))

(defn parse-board [nums]
  (let [positions (for [x (range 5) y (range 5)] [x y])]
    {:unmarked (zipmap nums positions)
     :marked #{}}))

(defn parse [file-name]
  (let [numbers-from-string #(map u/parse-long (re-seq #"\d+" %))
        [numbers-string & boards-strings] (map numbers-from-string (u/paragraphs 2021 file-name))
        boards (mapv parse-board boards-strings)]
    {:numbers numbers-string
     :boards boards}))

#_(parse "4min")

(defn mark [num board]
  (let [{:keys [marked unmarked]} board
        mark-pos (unmarked num)
        pos-marked (conj marked mark-pos)
        new-unmarked (dissoc unmarked num)]
    {:marked pos-marked :unmarked new-unmarked}))

#_(let [{:keys [boards]} (parse "4min")]
    (map #(mark 7 %) boards))

(defn bingoed? [{:keys [marked]}]
  (let [rows (frequencies (map first marked))
        cols (frequencies (map second marked))
        row-counts (set (vals rows))
        col-counts (set (vals cols))]
    (contains? (set/union row-counts col-counts) 5)))

(defn part1-reducer [boards num]
  (let [new-boards (map #(mark num %) boards)
        bingoed-boards (filter bingoed? new-boards)]
    (if (not-empty bingoed-boards)
      (reduced [(first bingoed-boards) num])
      new-boards)))

(defn part2-reducer [boards num]
  (let [new-boards (map #(mark num %) boards)
        bingoed-boards (remove bingoed? new-boards)]
    (if (empty? bingoed-boards)
      (reduced [(first new-boards) num])
      bingoed-boards)))

(defn solve [f {:keys [boards numbers]}]
  (let [[{:keys [unmarked]} last-num] (reduce f boards numbers)]
    (* (reduce + (keys unmarked)) last-num)))

; part 1
#_(solve part1-reducer (parse "4"))
#_(solve part2-reducer (parse "4"))
