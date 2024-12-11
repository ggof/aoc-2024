(ns aoc-2024.day11
  (:require
   [clojure.string :as str]))

(def for-stone
  (memoize
   (fn [stone n]
     (if (= 0 n) 1
         (if (= 0 stone)
           (for-stone 1 (dec n))
           (let [strstone (str stone)
                 is-even? (even? (count strstone))
                 half-len (/ (count strstone) 2)]
             (if is-even?
               (+ (for-stone (parse-long (str/join (take half-len strstone))) (dec n))
                  (for-stone (parse-long (str/join (drop half-len strstone))) (dec n)))
               (for-stone (* 2024 stone) (dec n)))))))))

(defn parse-and-solve-for [input times]
  (let [input  (map parse-long (str/split input #"\s"))]
    (transduce (map #(for-stone % times)) + input)))

(defn part-1 [input] (parse-and-solve-for input 25))

(defn part-2 [input] (parse-and-solve-for input 75))

(defn main [_]
  (let [input (slurp "inputs/day11.txt")]
    (println (part-1 input))
    (println (part-2 input))))
