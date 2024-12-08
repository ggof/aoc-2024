(ns aoc-2024.day05
  (:require
   [clojure.set :refer [intersection]]
   [clojure.string :as str]))

(defn split-parts [input]
  (let [[rules updates] (str/split input #"\n\n")
        rules (map #(map Integer/parseInt (str/split % #"\|")) (str/split-lines rules))
        updates (map #(map Integer/parseInt (str/split % #",")) (str/split-lines updates))]
    [rules updates]))

(defn append-into [acc [k v]]
  (update acc k conj v))

(defn rules->map [rules]
  (update-vals (reduce append-into {} rules) set))

(defn find-violations [rules line i]
  (let [before (set (take i line))
        elem (nth line i)
        not-allowed (rules elem)]
    (intersection not-allowed before)))

(defn respects-rules? [rules line]
  (->> line
       count
       range
       (map (partial find-violations rules line))
       (every? empty?)))

(defn middle-page [line]
  (nth line (/ (count line) 2)))

(defn part-1 [rules updates]
  (transduce (comp (filter (partial respects-rules? rules)) (map middle-page)) + updates))

(defn main [_]
  (let [input (slurp "inputs/day05.txt")
        [rules updates] (split-parts input)
        rules (rules->map rules)]
    (println (part-1 rules updates))))
