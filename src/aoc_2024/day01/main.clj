(ns aoc-2024.day01.main
  (:require
   clojure.java.io
   [clojure.string :as str]))

(defn str->int-pair [line]
  (take 2 (map Integer/parseInt (str/split line #"   "))))

(defn part-1 [lines]
  (->> lines
       (map str->int-pair)
       (apply map list)
       (map sort)
       (apply map list)
       (map (partial apply -))
       (map abs)
       (reduce +)))

(defn part-2 [lines]
  (let [[l1 l2] (apply map list (map str->int-pair lines))
        freq (frequencies l2)]
    (reduce + (map #(* % (or (freq %) 0)) l1))))

(defn main [_]
  (let [text (slurp "inputs/day01.txt")
        lines (str/split-lines text)]
    (println (part-1 (seq lines)))
    (println (part-2 lines))))
