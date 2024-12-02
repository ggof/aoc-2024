(ns aoc-2024.day02.main
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
  (let [coll (map #(str/split % #"\s") coll)
        coll (map #(map Integer/parseInt %) coll)]
    coll))

(defn part-1 [coll]
  (->> coll
       to-int-lists
       (filter ordered?)
       (filter safe?)
       count))

(defn main [_]
  (println (part-1 (str/split-lines (slurp "inputs/day02.txt")))))
