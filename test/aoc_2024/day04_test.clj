(ns aoc-2024.day04-test
  (:require
   [aoc-2024.day04 :refer [add-vec get-pos]]
   [clojure.test :refer :all]))

(deftest add-vec-tests
  (testing "adding 2 vecs is working"
    (is (= [5 5] (add-vec [4 4] [1 1])))
    (is (= [3 3] (add-vec [4 4] [-1 -1])))
    (is (= [3 5] (add-vec [4 4] [-1 1])))))

(deftest get-pos-tests
  (testing "returns the proper position"
    (is (= [4 4] (get-pos [4 4] [-1 -1] 0)))
    (is (= [3 4] (get-pos [4 4] [-1 0] 1)))
    (is (= [2 6] (get-pos [4 4] [-1 1] 2)))
    (is (= [4 7] (get-pos [4 4] [0 1] 3)))
    (is (= [4 4] (get-pos [4 4] [1 1] 0)))
    (is (= [5 4] (get-pos [4 4] [1 0] 1)))
    (is (= [6 2] (get-pos [4 4] [1 -1] 2)))
    (is (= [4 1] (get-pos [4 4] [0 -1] 3)))))

