(ns aoc-2024.day13)

(defn algebra [[ax ay bx by px py]]
  (let [ca (/ (- (* px by) (* py bx))
              (- (* ax by) (* ay bx)))
        cb (/ (- px (* ax ca)) bx)]
    [ca cb]))

(defn cost [[ca cb]]
  (+ (* 3 ca) cb))

(defn parse [input]
  (partition 6 (map parse-long (re-seq #"\d+" input))))

(defn part-1 [input]
  (let [input (parse input)]
    (transduce (comp (map algebra) (filter (partial every? int?)) (map cost)) + input)))

(defn add-10000000000000 [[ax ay bx by px py]]
  [ax ay bx by (+ 10000000000000 px) (+ 10000000000000 py)])

(defn part-2 [input]
  (let [input (parse input)]
    (transduce (comp (map add-10000000000000) (map algebra) (filter (partial every? int?)) (map cost)) + input)))

(defn main [_]
  (let [input (slurp "inputs/day13.txt")]
    (println (part-1 input))
    (println (part-2 input))))
