(ns bobisageek.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- slurp-res [resource-path]
  (io! (slurp (io/resource resource-path))))

(defn day-text 
  ([day] (day-text 2020 day))
  ([year day]
    (let [res-path (str "aoc" year "/" day ".txt")]
      (slurp-res res-path))))

(defn day-lines 
  ([day] (str/split-lines (day-text day)))
  ([year day] (str/split-lines (day-text year day))))

(defn rgx-extract-pieces [re lines]
  (let [extract (comp (partial drop 1) first (partial re-seq re))]
    (if (string? lines)
      (extract lines)
      (map extract lines))))

(def count-if (comp count filter))

(defn group-by-to [key-fn val-fn coll]
  (reduce (fn [acc el]
            (update acc (key-fn el) (fnil conj []) (val-fn el))) {} coll))

(defn paragraphs 
  ([day] (paragraphs 2020 day))
  ([year day] (str/split (day-text year day) #"(\r?\n)\1")))
