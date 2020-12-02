(ns bobisageek.aoc2020.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- resource-lines [resource-path]
  (str/split-lines (slurp (io/resource resource-path))))

(defn day-lines [day]
  (let [res-path (str "aoc2020/" day ".txt")]
    (resource-lines res-path)))
