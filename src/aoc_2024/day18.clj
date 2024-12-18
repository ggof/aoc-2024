(ns aoc-2024.day18
  (:require
   [aoc-2024.lib :as lib]
   [clojure.string :as str]))

(defn bfs
  [graph s e]
  (loop [queue   [[0 s]]
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

(defn in-grid? [size [x y]]
  (and (< x size) (< y size) (>= x 0) (>= y 0)))

(defn surroundings [size pos walls]
  (set (filter (every-pred (partial in-grid? size) (complement walls)) (map (partial lib/add-vec pos) [[-1 0] [0 -1] [1 0] [0 1]]))))

(defn create-grid [size walls]
  (into {} (for [y (range size)
                 x (range size)
                 :when (not (walls [x y]))]
             [[x y] (surroundings size [x y] walls)])))

(defn parse [input]
  (map #(map parse-long %) (map #(str/split % #",") (str/split-lines input))))

(defn part-1 [input]
  (let [walls (into #{} (take 1024 (parse input)))
        grid (create-grid 71 walls)]
    (bfs grid [0 0] [70 70])))

(def min-nb 1025)
(def max-nb 3450)

(defn half-dist [a b]
  (int (/ (- a b) 2)))

(defn part-2 [input]
  (let [all-walls (parse input)]
    (loop [last-safe min-nb
           nb max-nb]
      (let [walls (into #{} (take nb all-walls))
            grid (create-grid 71 walls)]
        (if (nil? (bfs grid [0 0] [70 70]))
          (if (not (nil? (bfs (create-grid 71 (into #{} (take (dec nb) all-walls))) [0 0] [70 70])))
            (nth all-walls (dec nb))
            (recur last-safe (- nb (half-dist nb last-safe))))
          (if (nil? (bfs (create-grid 71 (into #{} (take (inc nb) all-walls))) [0 0] [70 70]))
            (nth  all-walls nb)
            (recur nb (+ nb (half-dist nb last-safe)))))))))

(defn main [_]
  (let [input (slurp "inputs/day18.txt")]
    (println (part-1 input))
    (println (part-2 input))))
