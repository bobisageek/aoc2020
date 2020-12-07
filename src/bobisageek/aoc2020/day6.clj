(ns bobisageek.aoc2020.day6
  (:require [bobisageek.aoc2020.utils :as u]
            [clojure.string :as str]
            [clojure.set :as s]))

(defn parse-input [input]
  (as-> input $
    (str/split $ #"(\r?\n)\1")
    (map #(str/split-lines %) $)
    (map #(map set %) $)))

#_(parse-input (u/day-text "6"))

(defn aggregate [f surveys]
  (->> surveys
    (map (partial apply f))
    (map count)
    (apply +)))


#_(let [input (parse-input (u/day-text "6"))]
    (println (aggregate s/union input))
    (println (aggregate s/intersection input)))

