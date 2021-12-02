(ns bobisageek.aoc2020.day19
  (:require [bobisageek.utils :as u]
            [instaparse.core :as insta]
            [clojure.string :as str]))

; kind of a lazy day - instaparse can magic this up easily

(def min-rules "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"")
(def min-input "ababbb\nbababa\nabbbab\naaabbb\naaaabbb")

(def substitutions (assoc (zipmap (map char (range 48 58)) (map char (range 65 75)))
                     \: \= \return \;))

(defn part1 [rules-text input-lines]
  (let [p (insta/parser (str/escape rules-text substitutions))]
    (u/count-if vector? (map (partial insta/parse p) input-lines))))


;part 1
#_ (part1 (u/day-text "19-rules") (u/day-lines "19-input"))

; part 2
#_ (part1 (u/day-text "19-rules2") (u/day-lines "19-input"))
