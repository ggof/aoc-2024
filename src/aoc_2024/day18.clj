(ns aoc-2024.day18
  (:require
   [aoc-2024.lib :as lib]
   [clojure.string :as str]))

(defn find-neighbors [v g] (g v))

(defn visited? [v coll] (some #(= % v) coll))

(defn bfs
  "Traverses a graph in Breadth First Search (BFS)."
  [graph v]
  (loop [queue   (conj clojure.lang.PersistentQueue/EMPTY v) ;; Use a queue to store the nodes we need to explore
         visited []]                                         ;; A vector to store the sequence of visited nodes
    (if (empty? queue) visited                               ;; Base case - return visited nodes if the queue is empty
        (let [v           (peek queue)
              neighbors   (find-neighbors v graph)
              not-visited (filter (complement #(visited? % visited)) neighbors)
              new-queue   (apply conj (pop queue) not-visited)]
          (if (visited? v visited)
            (recur new-queue visited)
            (recur new-queue (conj visited v)))))))

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
