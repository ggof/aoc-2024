(ns aoc-2024.day10
  (:require
   [aoc-2024.lib :as lib]))

(def count-paths
  (memoize
   (fn [grid pos]
     (let [v (grid pos)
           ks (map (partial lib/add-vec pos) [[0 -1] [1 0] [0 1] [-1 0]])
           vs (map grid ks)
           ps (filter (comp not nil?) (zipmap ks vs))]
       (if (= 9 v)
         1
         (reduce
          +
          (for [[pp pv] ps]
            (if (= pv (inc v)) (count-paths grid pp) 0))))))))

(defn parse [input]
  (-> input lib/string->grid lib/grid->map (update-vals #(Character/digit % 10))))

(defn part-1 [input]
  (let [grid (parse input)
        starts (filter #(= 0 (grid (first %))) grid)
        scores (map (fn [[k]] [k (count-paths grid k)]) starts)]
    (println scores)
    (reduce + (map second scores))))

(def input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn main [_]
  (println (part-1 input)))
