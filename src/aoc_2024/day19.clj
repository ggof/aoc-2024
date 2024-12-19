(ns aoc-2024.day19
  (:require
   [clojure.string :as str]))

(defn get-next-queue [pattern towels q]
  (transduce
   (comp (filter (partial str/starts-with? pattern)) (map #(subs pattern (count %))))
   (completing #(cons %2 %1))
   (rest q)
   towels))

(defn can-obtain? [towels pattern]
  (loop [q [pattern]]
    (let [pattern (first q)]
      (cond
        (empty? q) false
        (empty? pattern) true
        :else (recur (get-next-queue pattern towels q))))))

(defn parse [input]
  (let [[towels patterns] (str/split input #"\n\n")
        towels (str/split towels #",\s")
        patterns (str/split-lines patterns)]
    [towels patterns]))

(defn part-1 [input]
  (let [[towels patterns] (parse input)]
    (count (filter (partial can-obtain? towels) patterns))))

(defn main [_]
  (let [input (slurp "inputs/day19.txt")]
    (println (part-1 input))))
