(ns bobisageek.aoc2020.day14
  (:require [bobisageek.utils :as u]))

(defn parse-mask [s]
  (let [parsed          (re-find #"mask = ([01X]+)$" s)
        bits            (reverse (second parsed))
        bits-and-values (group-by first
                          (map vector bits (iterate (partial * 2) 1)))
        zeroes          (reduce + (map second (bits-and-values \0)))
        ones            (reduce + (map second (bits-and-values \1)))]
    {0 zeroes 1 ones}))

(defn parse-mem [s]
  (let [[_ addr-s value-s] (re-find #"mem\[(\d+)\] = (\d+)$" s)]
    (mapv #(Integer/parseInt %) [addr-s value-s])))

(defn do-mask [mask-map value]
  (-> value
    (bit-or (mask-map 1))
    (bit-and-not (mask-map 0))))

(defn part1 [input]
  (loop [mask {}
         mem  {}
         [this-line & remaining] input]
    (cond
      (nil? this-line) mem
      (.startsWith this-line "mask") (recur (parse-mask this-line) mem remaining)
      (.startsWith this-line "mem") (let [[i v] (parse-mem this-line)
                                          v' (do-mask mask v)]
                                      (recur mask (assoc mem i v') remaining)))))

#_(->> (part1 (u/day-lines "14"))
    (map val)
    (reduce +))

(defn sums
  ([coll] (sums coll [0]))
  ([[f & r] out]
   (cond
     (nil? f) out
     :else (let [n (concat out (map (partial + f) out))]
             (recur r n)))))


(defn part2-parse-mask [s]
  (let [parsed          (re-find #"mask = ([01X]+)$" s)
        bits            (reverse (second parsed))
        bits-and-values (group-by first
                          (map vector bits (iterate (partial * 2) 1)))
        always          (reduce + (map second (bits-and-values \1)))
        maybes          (sums (map second (bits-and-values \X)))
        affected        (apply max maybes)]
    {:always always :maybe maybes :affected affected}))

(defn do-mask-2 [mask value]
  (map #(bit-or (bit-and-not (bit-or value (mask :always)) (mask :affected)) %) (mask :maybe)))

(defn part2 [input]
  (loop [mask {}
         mem  {}
         [this-line & remaining] input]
    (cond
      (nil? this-line) (reduce + (map val mem))
      (.startsWith this-line "mask") (let [new-mask (part2-parse-mask this-line)]
                                       (println new-mask)
                                       (recur new-mask mem remaining))
      (.startsWith this-line "mem") (let [[addr value] (parse-mem this-line)
                                          new-mem (do-mask-2 mask addr)]
                                      (println new-mem)
                                      (recur mask
                                        (merge mem (zipmap new-mem (repeat value)))
                                        remaining))
      :else nil)))
