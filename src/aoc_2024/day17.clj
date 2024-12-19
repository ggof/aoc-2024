(ns aoc-2024.day17
  (:require
   [clojure.string :as str]))

(defn combo [state operand]
  (nth [0 1 2 3 (:a state) (:b state) (:c state)] operand))

(defn bxl [state operand]
  (assoc state :pc (inc (:pc state)) :b (bit-xor (:b state) operand)))

(defn bst [state operand]
  (assoc state :pc (inc (:pc state)) :b (bit-and (combo state operand) 7)))

(defn jnz [state operand]
  (assoc state :pc (if (= 0 (:a state)) (inc (:pc state)) (long (/ operand 2)))))

(defn bxc [state _]
  (assoc state :pc (inc (:pc state)) :b (bit-xor (:b state) (:c state))))

(defn out [state operand]
  (assoc state :pc (inc (:pc state)) :out (conj (:out state) (bit-and (combo state operand) 7))))

(defn adv [state operand]
  (assoc state :pc (inc (:pc state)) :a (long (/ (:a state) (bit-shift-left 1 (combo state operand))))))

(defn bdv [state operand]
  (assoc state :pc (inc (:pc state)) :b (long (/ (:a state) (bit-shift-left 1 (combo state operand))))))

(defn cdv [state operand]
  (assoc state :pc (inc (:pc state)) :c (long (/ (:a state) (bit-shift-left 1 (combo state operand))))))

(defn parse [input]
  (let [[a b c & program] (map parse-long (re-seq #"\d+" input))]
    {:a a :b b :c c :program (vec (partition 2 program)) :pc 0 :out []}))

(def ops [adv bxl bst jnz bxc out bdv cdv])

(defn run-program [state]
  (loop [state state]
    (if (>= (:pc state) (count (:program state)))
      (:out state)
      (let [[f op] (nth (:program state) (:pc state))
            f (nth ops f)]
        (recur (f state op))))))

(defn part-1 [input]
  (str/join "," (run-program (parse input))))

(defn main [_]
  (let [input (slurp "inputs/day17.txt")]
    (println (part-1 input))))
