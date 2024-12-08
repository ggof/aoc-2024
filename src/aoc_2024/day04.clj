(ns aoc-2024.day04
  (:require
   [clojure.string :as str]))

(defn into-map [acc [k v]] (assoc acc k v))

(defn grid->map [g]
  (->> (for [y (range (count g))]
         (for [x (range (count (nth g y)))]
           [[x y] (nth (nth g y) x)]))
       (apply concat)
       (reduce into-map {})))

(def directions '([-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]))

(defn add-vec [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn get-pos [pos dir idx] (add-vec pos (map #(* idx %) dir)))

(defn xmas-in-direction? [pos dir grid]
  (let [x-pos (get-pos pos dir 0)
        m-pos (get-pos pos dir 1)
        a-pos (get-pos pos dir 2)
        s-pos (get-pos pos dir 3)]
    (and (= \X (grid x-pos))
         (= \M (grid m-pos))
         (= \A (grid a-pos))
         (= \S (grid s-pos)))))

(defn reducer [pos grid acc dir]
  (if (xmas-in-direction? pos dir grid) (+ 1 acc) acc))

(defn count-xmas-for [grid [pos l]]
  (if (= \X l)
    (reduce (partial reducer pos grid) 0 directions)
    0))

(defn count-all-xmas [grid]
  (reduce + (map (partial count-xmas-for grid) grid)))

(defn main [_]
  (let [grid (map seq (str/split-lines (slurp "inputs/day04.txt")))]
    (println (count-all-xmas (grid->map grid)))))
