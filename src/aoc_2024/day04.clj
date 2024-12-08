(ns aoc-2024.day04
  (:require
   [clojure.string :as str]))

(def directions '([-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]))

(defn into-map [acc [k v]] (assoc acc k v))

(defn grid->map [g]
  (->> (for [y (range (count g))]
         (for [x (range (count (nth g y)))]
           [[x y] (nth (nth g y) x)]))
       (apply concat)
       (reduce into-map {})))

(defn add-vec [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn get-pos [pos dir idx] (add-vec pos (map (partial * idx) dir)))

(defn xmas-in-direction? [grid pos dir]
  (let [dirs (map (partial get-pos pos dir) (range 4))]
    (= [\X \M \A \S] (map grid dirs))))


(defn count-xmas-for [grid [pos l]]
  (if (= \X l)
    (count (filter (partial xmas-in-direction? grid pos) directions))
    0))

(defn part-1 [grid]
  (reduce + (map partial (partial count-xmas-for grid) grid)))

(defn main [_]
  (let [grid (map seq (str/split-lines (slurp "inputs/day04.txt")))]
    (println (part-1 (grid->map grid)))))
