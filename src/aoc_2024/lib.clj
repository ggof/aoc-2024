(ns aoc-2024.lib 
  (:require
   [clojure.string :as str]))

(defn string->grid [s] (map seq (str/split-lines s)))

(defn grid->map [g]
  (->> (for [y (range (count g))
             x (range (count (nth g y)))]
         [[x y] (nth (nth g y) x)])
       (into {})))

(defn add-vec [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

