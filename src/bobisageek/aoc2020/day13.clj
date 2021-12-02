(ns bobisageek.aoc2020.day13
  (:require [bobisageek.utils :as u]
            [clojure.string :as str]))

(defn parse [[timestamp busses]]
  [(Integer/parseInt timestamp) (map #(Integer/parseInt %) (remove #{"x"} (str/split busses #",")))])

(defn part1 [[timestamp bus-seq]]
  (let [times     (map #(vector % (first (drop-while (partial > timestamp) (iterate (partial + %) %)))) bus-seq)
        best-bus  (apply min-key second times)
        wait-time (- (second best-bus) timestamp)]
    (* wait-time (first best-bus))))

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [^int a ^int b]
  (cond (zero? a) [(Math/abs b) 0 1]
        (zero? b) [(Math/abs a) 1 0]
        :else (loop [s  0
                     s0 1
                     t  1
                     t0 0
                     r  (Math/abs b)
                     r0 (Math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                      (- t0 (* q t)) t
                      (- r0 (* q r)) r))))))

(defn chinese_remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod     (apply * n)
        reducer  (fn [sum [n_i a_i]]
                   (let [p     (quot prod n_i)              ; p = prod / n_i
                         egcd  (extended-gcd p n_i)         ; Extended gcd
                         inv_p (second egcd)]
                     (println n_i a_i p egcd inv_p)
                     (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))]       ; Replaces the Python for loop to sum
    (mod sum-prod prod)))                                   ; Result line


#_(let [busses'  (as-> (u/day-lines "13") $
                   (second $)
                   (str/split $ #",")
                   (map-indexed (fn [offset increment] [increment offset]) $)
                   (remove #(= "x" (first %)) $))
        busses'' (mapv (fn [[increment offset]]
                         [(Integer/parseInt increment) offset]) busses')
        busses''' (map (fn [[a b]] [a (- a (mod b a))]) busses'')
        b4 (apply map vector busses''')]
    (apply chinese_remainder b4))

#_ (chinese_remainder [3 5 7] [2 3 2])


