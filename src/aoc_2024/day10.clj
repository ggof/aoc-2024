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
         [pos]
         (apply
          concat
          (for [[pp pv] ps :when (= pv (inc v))]
            (count-paths grid pp))))))))

(defn parse [input]
  (-> input lib/string->grid lib/grid->map (update-vals #(Character/digit % 10))))

(defn part-1 [input]
  (let [grid (parse input)
        starts (filter #(= 0 (grid (first %))) grid)]
    (transduce (comp (map first) (map (partial count-paths grid)) (map set) (map count)) + starts)))

(defn main [_]
  (println (part-1 (slurp "inputs/day10.txt"))))
