(ns bobisageek.aoc2020.day2
  (:require [bobisageek.aoc2020.utils :as u]))

; a regex to extract the useful pieces of the line
(def rgx #"^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$")
#_(u/rgx-extract-pieces rgx (u/day-lines "2min"))

; parse the lines into a seq of seqs of (min max character(as a set) string)
(defn parse [f]
  (let [lines (u/day-lines (str f))
        extracted-lines (u/rgx-extract-pieces rgx lines)
        ->int #(Integer/parseInt %)
        ops [->int ->int set identity]
        transform #(map (fn [o v] (o v)) ops %)]
    (map transform extracted-lines)))

#_(parse "2min")

; part 1
(defn valid-password-part-1? [[min max char-set pass]]
  (<= min (count (filter char-set pass)) max))
#_(->> (parse "2")
    (filter valid-password-part-1?)
    count)

; part 2
(defn valid-password-part-2? [[pos-1 pos-2 char-set pass]]
  (let [char-n #(nth pass (dec %))
        p (filter char-set (map #(char-n %) [pos-1 pos-2]))]
    (= 1 (count p))))

#_ (->> (parse "2")
     (filter valid-password-part-2?)
     count)
