(ns aoc-2024.lib 
  (:require
   [clojure.string :as str]))

(defn string->grid [s] (mapv vec (str/split-lines s)))

(defn grid->map [g]
  (->> (for [y (range (count g))
             x (range (count (nth g y)))]
         [[x y] (nth (nth g y) x)])
       (into {})))
