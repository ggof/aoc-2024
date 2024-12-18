(ns aoc-2024.day18
  (:require
   [aoc-2024.lib :as lib]
   [clojure.string :as str]))

(def ^:private inf (Long/MAX_VALUE))

(defn neighbors
  "Returns n's neighbors, optionally filtered if unvisited"
  ([g n] (get g n {}))
  ([g n uv] (select-keys (neighbors g n) uv)))

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs curr unvisited]
  (let [curr-cost (costs curr)]
    (reduce
     (fn [c [nbr nbr-cost]] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
     costs
     (neighbors g curr unvisited))))

(defn dijkstra
  "Returns a mapping of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a mapping of nodes to map of neighboring nodes and associated cost.
  Optionally, specify :target node to return only the min price for target"
  [g src & {:keys [target]}]
  (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
         curr src
         unvisited (disj (apply hash-set (keys g)) src)]
    (if (or (empty? unvisited) (= inf (costs curr)))
      costs
      (let [costs' (update-costs g costs curr unvisited)
            curr' (first (sort-by costs' unvisited))]
        (if (= target curr)
          (costs' target)
          (recur costs'
                 curr'
                 (disj unvisited curr')))))))

(defn in-grid? [size [x y]]
  (and (< x size) (< y size) (>= x 0) (>= y 0)))

(defn surroundings [size pos walls]
  (set (filter (every-pred (partial in-grid? size) #(not (walls %))) (map (partial lib/add-vec pos) [[-1 0] [0 -1] [1 0] [0 1]]))))

(defn create-grid [size walls]
  (into {} (for [y (range size)
                 x (range size)
                 :when (not (walls [x y]))]
             [[x y] (surroundings size [x y] walls)])))

(def input "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(defn main [_]
  (let [walls (into #{} (take 12 (map #(map parse-long %) (map #(str/split % #",") (str/split-lines input)))))
        walls (apply assoc (map #(list % 1) walls))
        grid (create-grid 7 walls)]
    (dijkstra grid [0 0] :target [6 6])))
