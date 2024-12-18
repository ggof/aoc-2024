(ns aoc-2024.day18
  (:require
   [aoc-2024.lib :as lib]
   [clojure.string :as str]))

(defn bfs
  [graph s e]
  (loop [queue   [[s]]
         visited #{}]
    (let [path        (first queue)
          v           (first path)
          neighbors   (graph v)
          not-visited (filter (complement visited) neighbors)
          next-paths  (map #(cons % path) not-visited)
          new-queue   (apply conj (rest queue) next-paths)]
      (cond
        (= e v) path
        (visited v) (recur (rest queue) visited)
        :else (recur new-queue (conj visited v))))))

(defn in-grid? [size [x y]]
  (and (< x size) (< y size) (>= x 0) (>= y 0)))

(defn surroundings [size pos walls]
  (set (filter (every-pred (partial in-grid? size) (complement walls)) (map (partial lib/add-vec pos) [[-1 0] [0 -1] [1 0] [0 1]]))))

(defn create-grid [size walls]
  (into {} (for [y (range size)
                 x (range size)
                 :when (not (walls [x y]))]
             [[x y] (surroundings size [x y] walls)])))

(defn parse [input nb-bytes]
  (into #{} (take nb-bytes (map #(map parse-long %) (map #(str/split % #",") (str/split-lines input))))))

; (defn write-mase [grid]
;   (spit "day18.txt" (str/join (for [y (range 71)
;                                     x (range 71)]
;                                 (cond
;                                   (and (= x 70) (grid [x y])) ".\n"
;                                   (grid [x y]) "."
;                                   (= x 70) "#\n"
;                                   :else "#")))))
(defn part-1 [input]
  (let [walls (parse input 1024)
        grid (create-grid 71 walls)
        path (bfs grid [0 0] [70 70])]

    ; (write-mase grid)
    (- (count path) 1)))

(defn main [_]
  (let [input (slurp "inputs/day18.txt")]
    (println (part-1 input))))
