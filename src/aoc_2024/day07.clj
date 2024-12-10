(ns aoc-2024.day07
  (:require
   [clojure.string :as str]))

(defn check-calibration [ops target acc [hd & tl]]
  (if (empty? tl)
    (some #(= target (% acc hd)) ops)
    (some #(check-calibration ops target (% acc hd) tl) ops)))

(defn is-good-calibration? [ops [target xs]] (check-calibration ops target 0 xs))

(defn parse-line [line]
  (let [[target xs] (str/split line #": ")
        xs (str/split xs #" ")]
    [(parse-long target) (map parse-long xs)]))

(defn parse [input]
  (map parse-line (str/split-lines input)))

(defn part-1 [input]
  (transduce (comp (filter (partial is-good-calibration? [+ *])) (map first)) + (parse input)))

(defn cc [a b] (parse-long (str a b)))

(defn part-2 [input]
  (transduce (comp (filter (partial is-good-calibration? [+ * cc])) (map first)) + (parse input)))

(defn main [_]
  (let [input  (slurp "inputs/day07.txt")]
    (println (part-1 input))
    (println (part-2 input))))
