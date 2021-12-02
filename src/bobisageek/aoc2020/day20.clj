(ns bobisageek.aoc2020.day20
  (:require [bobisageek.utils :as u]
            [instaparse.core :as insta]
            [clojure.set :as set]))

(def parser (insta/parser
              "<S> = Tile*
             <LB> = #'\r?\n'
             <Tile> = Label Rows <LB?>
             Rows = Row+
             <Label> = <'Tile '> Tile_Num <':'> <LB>
             <Tile_Num> = #'[0-9]+'
             <Row> = Chars
             Chars = Char+ <LB>
             <Char> = '#' | '.'"))

#_(insta/parse parser (u/day-text "20min"))

(defn build-tile-map [input]
  (let [parsed-tiles (insta/parse parser input)
        tile-map     (map (fn [[t-num rows]]
                            (vector (Integer/parseInt t-num)
                              (mapv (comp vec (partial drop 1)) (drop 1 rows))))
                       (partition 2 parsed-tiles))]
    (into {} tile-map)))

#_(build-tile-map (u/day-text "20min"))

(defn edges [vvs]
  (let [edges ((juxt first last (partial mapv first) (partial mapv last)) vvs)
        rev-edges (map (comp vec reverse) edges)]
    (set (concat edges rev-edges))))

#_ (edges [[1 2 3] [4 5 6] [7 8 9]])

; part 1

#_(let [a (build-tile-map (u/day-text "20"))
        b (into {} (map (fn [[k v]] [k (edges v)]) a))]
    (->> (for [k (keys b)
               :let [ss (vals (dissoc b k))]]
           [k (apply set/difference (b k) ss)])
      (filter #(= 4 (count (second %))))
      (map first)
      (apply *)))

(defn rotate-cw [vvs]
  (mapv (comp vec reverse) (apply (partial map vector) vvs)))

#_(take 5 (iterate rotate-cw [[\# \. \.] [\. \# \.] [\. \# \.]]))
