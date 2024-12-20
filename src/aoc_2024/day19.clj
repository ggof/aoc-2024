(ns aoc-2024.day19
  (:require
   [clojure.string :as str]))

(def count-possible
  (memoize
   (fn [towels pattern]
     (if (empty? pattern) 1
         (transduce
          (comp (filter (partial str/starts-with? pattern))
                (map #(subs pattern (count %)))
                (map (partial count-possible towels)))
          + towels)))))

(defn parse [input]
  (let [[towels patterns] (str/split input #"\n\n")
        towels (str/split towels #",\s")
        patterns (str/split-lines patterns)]
    [towels patterns]))

(defn part-1 [input]
  (let [[towels patterns] (parse input)]
    (count (filter #(not= 0 (count-possible towels %)) patterns))))

(defn part-2 [input]
  (let [[towels patterns] (parse input)]
    (transduce (map (partial count-possible towels)) + patterns)))

(defn main [_]
  (let [input (slurp "inputs/day19.txt")]
    (println (part-1 input))
    (println (part-2 input))))
