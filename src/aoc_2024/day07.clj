(ns aoc-2024.day07
  (:require
   [clojure.string :as str]))

(defn check-calibration [target acc [hd & tl]]
  (if (empty? tl)
    (or
     (= target (+ acc hd))
     (= target (* acc hd)))
    (or
     (check-calibration target (+ acc hd) tl)
     (check-calibration target (* acc hd) tl))))

(defn is-good-calibration? [[target xs]] (check-calibration target 0 xs))

(defn parse-line [line]
  (let [[target xs] (str/split line #": ")
        xs (str/split xs #" ")]
    [(parse-long target) (map parse-long xs)]))

(defn parse [input]
  (map parse-line (str/split-lines input)))

(defn part-1 [input]
  (transduce (comp (filter is-good-calibration?) (map first)) + (parse input)))

(defn main [_]
  (println (part-1 (slurp "inputs/day07.txt"))))
