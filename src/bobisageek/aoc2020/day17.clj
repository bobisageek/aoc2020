(ns bobisageek.aoc2020.day17)

(def min-input
  #{[1 0 0] [2 1 0] [0 2 0] [1 2 0] [2 2 0]})

(def puzzle-input
  #{[2 0 0] [5 0 0] [7 0 0]
    [0 1 0] [1 1 0] [3 1 0] [6 1 0]
    [0 2 0] [5 2 0]
    [1 3 0] [4 3 0] [5 3 0] [6 3 0] [7 3 0]
    [5 4 0]
    [3 5 0] [4 5 0]
    [1 6 0] [3 6 0] [4 6 0] [7 6 0]
    [1 7 0] [3 7 0] [5 7 0] [7 7 0]})

(defn conjall [seqs vals]
  (for [s seqs
        v vals]
    (conj s v)))

(defn neighbors [point]
  (let [deltas [-1 0 1]
        out    (reduce #(conjall %1 (map + deltas (repeat %2))) [[]] point)]
    (remove #(= point %) out)))

(defn next-gen [f active-points]
  (let [all-neighbors (mapcat f active-points)
        freqs         (frequencies all-neighbors)
        survivors     (keep
                        (fn [[point count]]
                          (when (or (and (active-points point)
                                      (<= 2 count 3))
                                  (= 3 count)) point)) freqs)]
    (set survivors)))


; part 1
#_(time (count (nth (iterate (partial next-gen neighbors) puzzle-input) 6)))

;part 2
#_(time (count (nth (iterate (partial next-gen neighbors) (set (map #(conj % 0) puzzle-input))) 6)))
