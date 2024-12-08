(ns aoc-2024.day04
  (:require
   [aoc-2024.lib :as lib]))

(def directions '([-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]))

(defn get-pos [pos dir idx] (lib/add-vec pos (map (partial * idx) dir)))

(defn xmas-in-direction? [grid pos dir]
  (let [dirs (map (partial get-pos pos dir) (range 4))]
    (= [\X \M \A \S] (map grid dirs))))

(defn count-xmas-for [grid [pos l]]
  (if (= \X l)
    (count (filter (partial xmas-in-direction? grid pos) directions))
    0))

(defn part-1 [input]
  (->> input
       lib/string->grid
       lib/grid->map)
  #(reduce + (map partial (partial count-xmas-for %) %)))

(defn main [_]
  (let [input (slurp "inputs/day04.txt")]
    (println (part-1 input))))
