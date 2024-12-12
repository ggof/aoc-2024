(ns aoc-2024.day08
  (:require
   [aoc-2024.lib :as lib]))

(defn in-bounds? [max-x max-y [x y]] (and (<= 0 x) (< x max-x) (<= 0 y) (< y max-y)))

(defn get-antinodes [max-x max-y c a]
  (let [c-a (lib/sub-vec c a)
        a-c (lib/sub-vec a c)
        p1 (lib/add-vec c c-a)
        p2 (lib/add-vec a a-c)]
    (filter (partial in-bounds? max-x max-y) [p1 p2])))

(defn antinodes-for [pos antennas nodes max-x max-y]
  (let [new-nodes (transduce (map (partial get-antinodes max-x max-y pos)) concat antennas)]
    (apply conj nodes new-nodes)))

(defn solve [chars max-x max-y]
  (loop [[hd & tl] chars
         ants {}
         nodes #{}
         [x y] [0 0]]
    (cond
      (empty? tl) (count (antinodes-for [x y] (ants hd) nodes max-x max-y))
      (= \newline hd) (recur tl ants nodes [0 (inc y)])
      (= \. hd) (recur tl ants nodes [(inc x) y])
      :else (let [antennas (ants hd)
                  new-nodes (antinodes-for [x y] antennas nodes max-x max-y)]
              (recur tl (update ants hd conj [x y]) new-nodes [(inc x) y])))))

(defn day-1 [input]
  (solve (seq input) 50 50))

(defn main [_]
  (let [input (slurp "inputs/day08.txt")]
    (println (day-1 input))))
