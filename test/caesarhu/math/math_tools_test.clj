(ns caesarhu.math.math-tools-test
  (:require [clojure.test :refer :all]
            [caesarhu.math.math-tools :refer :all]))

(deftest square-test
  (testing "square? test"
    (doseq [x (repeatedly 100 #(rand-int 1000000))]
      (is (square? (* x x))))))

(deftest digits-test
  (testing "digits test"
    (let [x 987]
      (is (= '(9 8 7) (digits x)))
      (is (= x (digits->number [9 8 7])))
      (is (= '(2 6 1 0) (digits x 7)))
      (is (= x (digits->number [2 6 1 0] 7))))))

(deftest factorial-test
  (testing "factorial test"
    (is (= 1 (factorial 0)))
    (is (= 1 (factorial 1)))
    (is (= (reduce * (range 1 11)) (factorial 10)))))

(deftest binomial-test
  (testing "binomial test"
    (is (= 3 (binomial 3 2)))
    (is (= 120 (binomial 10 7)))))

(deftest gcd*-test
  (testing "gcd* test"
    (is (= 7 (gcd* 28 35 70)))
    (is (= 2 (gcd* 4 748 14 32 18)))))

(deftest lcm*-test
  (testing "lcm* test"
    (is (= 140 (lcm* 28 35 70)))
    (is (= 376992 (lcm* 4 748 14 32 18)))))

(deftest palindrome?-test
  (testing "palindrome? test"
    (is (true? (palindrome? 121)))
    (is (true? (palindrome? 906609)))))

(deftest sqrt-continued-fraction-test
  (testing "sqrt-continued-fraction test"
    (is (= [2 1 1 1 4] (sqrt-continued-fraction 7)))
    (is (= [4 2 1 3 1 2 8] (sqrt-continued-fraction 19)))))