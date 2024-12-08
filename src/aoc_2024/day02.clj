(ns aoc-2024.day02
  (:require [clojure.string :as str]))

(defn ordered? [coll]
  (or (apply < coll) (apply > coll)))

(defn between-1-3 [[a b]]
  (let [x (abs (- a b))]
    (and (> x 0) (< x 4))))

(defn safe? [coll]
  (->> coll
       (partition 2 1)
       (every? between-1-3)))

(defn to-int-lists [coll]
  (->> coll
       (map #(str/split % #"\s"))
       (map #(partial map Integer/parseInt %))))

(defn part-1 [coll]
  (->> coll
       to-int-lists
       (filter (every-pred ordered? safe?))
       count))

(defn drop-nth [n coll]
  (concat (take n coll) (drop (inc n) coll)))

(defn any-permutation-safe? [nbs]
  (let [permutations (for [i (range (count nbs))] (drop-nth i nbs))]
    (some (every-pred ordered? safe?) permutations)))

(defn part-2 [coll]
  (->> coll
       to-int-lists
       (filter any-permutation-safe?)
       count))

(defn main [_]
  (let [lines  (str/split-lines (slurp "inputs/day02.txt"))]
    (println (part-1 lines))
    (println (part-2 lines))))
