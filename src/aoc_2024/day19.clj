(ns aoc-2024.day19
  (:require
   [clojure.string :as str]))

(defn can-obtain? [towels pattern]
  (if (empty? pattern)
    true
    (->> towels
         (filter (partial str/starts-with? pattern))
         (some #(can-obtain? towels (subs pattern (count %)))))))

(defn parse [input]
  (let [[towels patterns] (str/split input #"\n\n")
        towels (str/split towels #",\s")
        patterns (str/split-lines patterns)]
    [towels patterns]))

(defn part-1 [input]
  (let [[towels patterns] (parse input)]
    (count (filter (partial can-obtain? towels) patterns))))

(defn main [_]
  (let [input (slurp "inputs/day19.txt")]
    (println (part-1 input))))
