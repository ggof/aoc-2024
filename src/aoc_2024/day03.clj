(ns aoc-2024.day03
  (:require
   [clojure.string :as string]))

(defn get-matches [line]
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" line))

(defn numbers [match]
  (map Integer/parseInt (drop 1 match)))

(defn part-1 [line]
  (transduce (comp (map numbers) (map (partial apply *))) + (get-matches line)))

(defn remove-disabled-instructions [line]
  (string/replace line #"(?s)don't\(\).+?do\(\)" ""))

(defn part-2 [line] (part-1 (remove-disabled-instructions line)))

(defn main [_]
  (let [line  (slurp "inputs/day03.txt")]
    (println (part-1 line))
    (println (part-2 line))))
