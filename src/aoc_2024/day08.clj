(ns aoc-2024.day08)

(def max-x 50)
(def max-y 50)

(defn in-bounds? [[x y]] (and (<= 0 x) (< x max-x) (<= 0 y) (< y max-y)))

(defn get-first-antinodes [c a]
  (let [c-a (mapv + c a)
        a-c (mapv + a c)
        p1 (mapv + c c-a)
        p2 (mapv + a a-c)]
    (filter in-bounds? [p1 p2])))

(defn antinodes-for [get-antinodes pos antennas nodes]
  (let [new-nodes (transduce (map (partial get-antinodes pos)) concat antennas)]
    (apply conj nodes new-nodes)))

(defn solve [get-antinodes chars]
  (loop [[hd & tl] chars
         ants {}
         nodes #{}
         [x y] [0 0]]
    (cond
      (empty? tl) (count (antinodes-for get-antinodes [x y] (ants hd) nodes))
      (= \newline hd) (recur tl ants nodes [0 (inc y)])
      (= \. hd) (recur tl ants nodes [(inc x) y])
      :else (let [antennas (ants hd)
                  new-nodes (antinodes-for get-antinodes [x y] antennas nodes)]
              (recur tl (update ants hd conj [x y]) new-nodes [(inc x) y])))))

(defn part-1 [input]
  (solve get-first-antinodes (seq input)))

(defn in-direction [p d]
  (loop [acc []
         pos p]
    (if (in-bounds? pos)
      (recur (conj acc pos) (mapv + pos d))
      acc)))

(defn get-all-antinodes [c a]
  (let [c-a (mapv - c a)
        a-c (mapv - a c)]
    (concat (in-direction c a-c) (in-direction a c-a))))

(defn part-2 [input]
  (solve get-all-antinodes (seq input)))

(defn main [_]
  (let [input (slurp "inputs/day08.txt")]
    (println (part-1 input))
    (println (part-2 input))))
