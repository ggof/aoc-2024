(ns aoc-2024.day03)

(defn get-matches [line]
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" line))

(defn numbers [match]
  (map Integer/parseInt (drop 1 match)))

(defn part-1 [line]
  (transduce (comp (map numbers) (map (partial apply *))) + (get-matches line)))

(defn main [_]
  (let [line (slurp "inputs/day03.txt")]
    (println (part-1 line))))
