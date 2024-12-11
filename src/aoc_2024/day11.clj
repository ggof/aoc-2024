(ns aoc-2024.day11
  (:require
   [clojure.string :as str]))

(defn rule-1 [stone] (if (= 0 stone) 1 false))

(defn rule-2 [stone]
  (let [strstone (str stone)
        half-len (/ (count strstone) 2)]
    (if (even? (count strstone))
      [(parse-long (str/join (take half-len strstone))) (parse-long (str/join (drop half-len strstone)))]
      false)))

(defn rule-3 [stone] (* 2024 stone))

(defn by-rules [stone]
  (if-let [nextstone (rule-1 stone)]
    nextstone
    (if-let [nextstone (rule-2 stone)]
      nextstone
      (rule-3 stone))))

(defn stones-after [stones blinks]
  (loop [stones stones
         blinks blinks]
    (println "blink" blinks)
    (println "we have" (count stones) "stones")
    (if
     (= 0 blinks)
      (count stones)
      (recur (flatten (map by-rules stones)) (dec blinks)))))

(defn part-1 [input]
  (-> (str/split input #" ")
      (#(map parse-long %))
      (stones-after 25)))

(defn main [_]
  (let [input (slurp "inputs/day11.txt")]
    (println (part-1 input))))
