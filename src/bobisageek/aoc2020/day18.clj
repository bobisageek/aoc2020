(ns bobisageek.aoc2020.day18
  (:require [bobisageek.utils :as u]
            [clojure.edn :as edn]))

(defn domath [l]
  (cond
    (number? l) l
    (#{+ *} l) "error"
    :else
    (let [[lhs op rhs & r] l
          ops {'+ + '* *}]
      (if (nil? op)
        (domath lhs)
        (recur (conj r ((ops op) (domath lhs) (domath rhs))))))))

; part 1
#_(->> (u/day-lines "18")
    (map #(edn/read-string (str "(" % ")")))
    (map domath)
    (reduce +))

(defn p [l]
  (println "l1: " l)
  (cond
    (number? l) l
    (#{+ *} l) "error"
    :else
    (let [[lhs op rhs & r] l]
      (println "l: " l)
      (cond
        (nil? op) l
        (= op '+) (p (conj r (list (p lhs) op (p rhs))))
        (= op '*) (list (p lhs) op (p (if (seq r)
                                        (conj r rhs)
                                        rhs)))))))


; part 2 - being lazy and just parenthesizing all the additions
#_(->> (u/day-lines "18")
    (map #(edn/read-string (str "(" % ")")))
    (map p)
    (map domath)
    (reduce +))
