(ns aoc-2024.day12
  (:require
   [aoc-2024.lib :as lib]
   [clojure.set :as set]))

(def max-x 50)
(def max-y 50)

(defn in-bounds? [[x y]] (and (<= 0 x) (< x max-x) (<= 0 y) (< y max-y)))

(defn find-whole-area [m p a]
  (let [v (m p)
        surroundings (map (partial lib/add-vec) [[0 -1] [0 1] [-1 0] [1 0]])
        pos-to-add (filter (every-pred in-bounds? #(= v (m %))) surroundings)
        next-a (set/union a p)]

    (if (empty? pos-to-add) 
      a 
      (apply set/union (map #(find-whole-area m % next-a) pos-to-add)))))
