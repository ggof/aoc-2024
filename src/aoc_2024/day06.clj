(ns aoc-2024.day06 
  (:require
   [aoc-2024.lib :as lib]))

(def next-dir {[-1 0] [0 -1], [0 -1] [1 0], [1 0] [0 1],[0 1] [-1 0]})

(defn is-start [[_ v]] (= \^ v))

(defn find-start [grid] (ffirst (filter is-start grid)))

(defn move-guard [grid]
  (loop [pos (find-start grid)
         dir [0 -1]
         vis #{pos}]
    (let [npos (mapv + pos dir)]
      (case (grid npos)
        nil (conj vis pos)
        \^ (recur npos dir (conj vis pos))
        \. (recur npos dir (conj vis pos))
        \# (let [ndir (next-dir dir)]
             (recur (mapv + pos ndir) ndir (conj vis pos)))))))

(defn part-1 [input]
  (->> input
       lib/string->grid
       lib/grid->map
       move-guard
       count))

(defn main [_]
  (let [input (slurp "inputs/day06.txt")]
    (println (part-1 input))))
