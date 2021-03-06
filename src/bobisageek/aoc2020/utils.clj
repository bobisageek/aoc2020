(ns bobisageek.aoc2020.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- slurp-res [resource-path]
  (io! (slurp (io/resource resource-path))))

(defn day-text [day]
  (let [res-path (str "aoc2020/" day ".txt")]
    (slurp-res res-path)))

(defn day-lines [day]
  (str/split-lines (day-text day)))

(defn rgx-extract-pieces [re lines]
  (let [extract (comp (partial drop 1) first (partial re-seq re))]
    (if (string? lines)
      (extract lines)
      (map extract lines))))

(def count-if (comp count filter))

(defn paragraphs [day]
  (str/split (day-text day) #"(\r?\n)\1"))
