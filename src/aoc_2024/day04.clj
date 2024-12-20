(ns aoc-2024.day04
  (:require
   [aoc-2024.lib :as lib]))

(def directions '([-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]))

(defn get-pos [pos dir idx] (mapv + pos (map (partial * idx) dir)))

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
       lib/grid->map
       (#(reduce + (map (partial count-xmas-for %) %)))))

(defn is-xmas? [grid pos]
  (let [l (map grid (map (partial mapv + pos) [[-1 -1] [-1 1]]))
        r (map grid (map (partial mapv + pos) [[1 -1] [1 1]]))
        t (map grid (map (partial mapv + pos) [[-1 -1] [1 -1]]))
        b (map grid (map (partial mapv + pos) [[-1 1] [1 1]]))]
    (or
     (and (= l [\M \M]) (= r [\S \S]))
     (and (= r [\M \M]) (= l [\S \S]))
     (and (= b [\M \M]) (= t [\S \S]))
     (and (= t [\M \M]) (= b [\S \S])))))

(defn is-cross? [grid [pos l]]
  (and (= \A l) (is-xmas? grid pos)))

(defn part-2 [input]
  (->> input
       lib/string->grid
       lib/grid->map
       (#(filter (partial is-cross? %) %))
       count))

(defn main [_]
  (let [input (slurp "inputs/day04.txt")]
    (println (part-1 input))
    (println (part-2 input))))
