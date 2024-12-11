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

(def for-stone
  (memoize
   (fn [stone n]
     (if (= 0 n)
       1
       (if (= 0 stone)
         (for-stone 1 (dec n))
         (let [strstone (str stone)
               is-even? (even? (count strstone))
               half-len (/ (count strstone) 2)]
           (if is-even?
             (+ (for-stone (parse-long (str/join (take half-len strstone))) (dec n))
                (for-stone (parse-long (str/join (drop half-len strstone))) (dec n)))
             (for-stone (* 2024 stone) (dec n)))))))))

(defn stones-after [stones blinks]
  (loop [stones stones
         blinks blinks]
    (println "blinks left:" blinks "we have" (count stones) "stones")
    (if
     (= 0 blinks)
      (count stones)
      (recur (flatten (map by-rules stones)) (dec blinks)))))

(defn part-1 [input]
  (-> (str/split input #" ")
      (#(map parse-long %))
      (stones-after 25)))

(defn part-2 [input]
  (let [input  (map parse-long (str/split input #" "))]
    (transduce (map #(for-stone % 25)) + input)))

(defn main [_]
  (let [input (slurp "inputs/day11.txt")]
    (println (part-2 input))))
    ; (println (part-2 input))))
