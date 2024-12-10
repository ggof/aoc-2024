(ns aoc-2024.day10
  (:require
   [aoc-2024.lib :as lib]))

(def count-paths (memoize (fn [grid pos cnt])))

(defn part-1 [input]
  (let [grid (-> input lib/string->grid lib/grid->map)
        starts (filter #(= 0 (grid %)) grid)]
    (reduce + (map #(count-paths grid (first %) 0) starts))))

(defn main [_])
