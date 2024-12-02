(ns aoc-2024.day02.main
  (:require [clojure.string :as str]))

(defn tee [x]
  (println x)
  x)

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

(defn sum-safes [coll]
  (->> coll
       to-int-lists
       (filter ordered?)
       (filter safe?)
       count))

(def var1 "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn main [_]
  (let [input  (slurp "inputs/day02.txt")]
    (println (sum-safes (str/split-lines input)))))

