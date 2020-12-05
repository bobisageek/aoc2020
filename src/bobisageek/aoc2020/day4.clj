(ns bobisageek.aoc2020.day4
  (:require [bobisageek.aoc2020.utils :as u]
            [clojure.string :as str]))

(defn split-passports [text]
  (as-> text $
    (str/split $ #"(\r?\n)\1")
    (map #(str/split % #":|\s+") $)
    (map (comp #(map vec %) #(partition 2 %)) $)
    (map #(into {} %) $)))

#_(split-passports (u/day-text "4min"))

(def req-keys ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn valid-passport-part-1? [passport]
  (every? (set (keys passport)) req-keys))

(defn part1 [input]
  (let [passports (split-passports input)]
    (->> (filter valid-passport-part-1? passports)
      count)))

#_(part1 (u/day-text "4"))

(defn year-in-range [min max s]
  (and (re-matches #"[0-9]{4}" s)
    (<= min (Integer/parseInt s) max)))

(defn check-height [h]
  (when-let [[cm in] (->> h
                       (re-matches #"(?:(1[5-9]\d)cm|([5-7]\d)in)")
                       (drop 1)
                       seq)]
    (cond
      cm (<= 150 (Integer/parseInt cm) 193)
      in (<= 59 (Integer/parseInt in) 76))))


(def passport-validations {"byr" (partial year-in-range 1920 2002)
                           "iyr" (partial year-in-range 2010 2020)
                           "eyr" (partial year-in-range 2020 2030)
                           "hgt" (partial check-height)
                           "hcl" (partial re-matches #"#[0-9a-f]{6}")
                           "ecl" (partial #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
                           "pid" (partial re-matches #"[0-9]{9}")})

(defn validate-passport-fields [p]
  (let [trimmed (select-keys p req-keys)]
    (every? identity (map #((passport-validations %) (trimmed %)) (keys trimmed)))))

(defn part2 [input]
  (let [passports (split-passports input)]
    (->> (filter #(and (valid-passport-part-1? %) (validate-passport-fields %)) passports)
      count)))



#_(->> (part2 (u/day-text "4")))






