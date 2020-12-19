(ns bobisageek.aoc2020.day16
  (:require [bobisageek.aoc2020.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-rule [s]
  (let [[name & nums] (u/rgx-extract-pieces #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" s)
        [b1 e1 b2 e2] (map #(Integer/parseInt %) nums)
        possibles (apply set/union (map set [(range b1 (inc e1)) (range b2 (inc e2))]))]
    [name possibles]))

(defn parse-rules [s]
  (into {} (map parse-rule (str/split-lines s))))

(defn number-fields [s]
  (mapv #(Integer/parseInt %) (str/split s #",")))

(defn parse [f]
  (let [[rules-txt your-txt nrby-txt] (u/paragraphs f)
        rules        (parse-rules rules-txt)
        your-ticket  (number-fields (second (str/split-lines your-txt)))
        nrby-tickets (map number-fields (drop 1 (str/split-lines nrby-txt)))]
    {:rules rules :your your-ticket :nrby nrby-tickets}))

(defn part1 [input]
  (let [valid-values       (apply set/union (vals (:rules input)))
        nrby-ticket-values (flatten (:nrby input))
        scan-errors        (remove valid-values nrby-ticket-values)]
    (reduce + scan-errors)))

(defn part2 [input]
  (let [{:keys [rules your nrby]} input
        num-of-fields           (count rules)
        new-rules               (set/map-invert rules)
        valid-values            (apply set/union (keys new-rules))
        filtered-nrby           (filter #(every? valid-values %) nrby)
        transposed-tickets      (apply map vector filtered-nrby)
        filter-fields           (fn [t] (set (mapv second (filter #(every? (key %) t) new-rules))))
        field-possibilities     (mapv filter-fields transposed-tickets)
        field-positions         (loop [maybes field-possibilities
                                       outs   {}]
                                  (if (every? #(contains? outs %) (range num-of-fields))
                                    outs
                                    (let [new-maybes (map #(set/difference % (set (vals outs))) maybes)
                                          new-outs   (into outs (keep-indexed #(when (= 1 (count %2)) [%1 (first %2)]) new-maybes))]
                                      (recur new-maybes new-outs))))
        departure-field-indexes (keep (fn [[index field-name]]
                                        (when (.startsWith field-name "departure") index)) field-positions)
        your-ticket-values      (map #(your %) departure-field-indexes)]
    [(reduce * your-ticket-values) (sort-by key field-positions)]))

#_(part2 (parse "16"))


