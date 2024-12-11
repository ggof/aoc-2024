(ns aoc-2024.day08)

(defn count-antinodes-for [c antennas max-x max-y] 0)

(defn solve [chars max-x max-y]
  (loop [[hd & tl] chars
         ants {}
         cnt 0
         [x y] [0 0]]
    (cond
      (= \newline hd) (recur tl ants cnt [0 (inc y)])
      (= \. hd) (recur tl ants cnt [(inc x) y])
      :else (let [antennas (ants hd)
                  next-cnt (count-antinodes-for hd antennas max-x max-y)]
              (recur tl (update ants hd conj [x y]) (+ cnt next-cnt) [(inc x) y])))))

(defn main [_])
