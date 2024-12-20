(ns aoc-2024.day18
  (:require [clojure.string :as str]))

(defn bfs
  [graph s e]
  (loop [queue   [[1 s]]
         visited #{}]
    (let [[c n]       (first queue)
          neighbors   (graph n)
          not-visited (filter (complement visited) neighbors)
          next-nodes  (map (fn [n] [(inc c) n]) not-visited)
          new-queue   (sort-by first (apply conj (rest queue) next-nodes))]
      (cond
        (= e n) c
        (nil? n) nil
        (visited n) (recur (rest queue) visited)
        :else (recur new-queue (conj visited n))))))

(defn in-grid? [size [x y]] (and (< x size) (< y size) (>= x 0) (>= y 0)))

(defn in-grid-and-not-wall? [size walls] (every-pred (partial in-grid? size) (complement walls)))

(defn surroundings [size pos walls]
  (set (filter (in-grid-and-not-wall? size walls) (map (partial mapv + pos) [[-1 0] [0 -1] [1 0] [0 1]]))))

(defn create-grid [size walls]
  (into {} (for [y (range size)
                 x (range size)
                 :when (not (walls [x y]))]
             [[x y] (surroundings size [x y] walls)])))

(defn parse [input]
  (map #(map parse-long %) (map #(str/split % #",") (str/split-lines input))))

(defn grid-with-walls [walls nb] (create-grid 71 (set (take nb walls))))

(defn part-1 [input] (bfs (grid-with-walls (parse input) 1024) [0 0] [70 70]))

(defn half-dist [a b] (long (/ (- a b) 2)))

(defn binary-search [all-walls btm top]
  (loop [last-safe btm
         nb top]
    (if (nil? (bfs (grid-with-walls all-walls nb) [0 0] [70 70]))
      (if (not (nil? (bfs (grid-with-walls all-walls (dec nb)) [0 0] [70 70])))
        (nth all-walls (dec nb))
        (recur last-safe (- nb (half-dist nb last-safe))))
      (if (nil? (bfs (grid-with-walls all-walls (inc nb)) [0 0] [70 70]))
        (nth  all-walls nb)
        (recur nb (+ nb (half-dist nb last-safe)))))))

(defn part-2 [input] (binary-search (parse input) 1025 3450))

(defn main [_]
  (let [input (slurp "inputs/day18.txt")]
    (println (part-1 input))
    (println (part-2 input))))
