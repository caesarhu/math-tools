(ns caesarhu.math.polynomial-test
  (:require [clojure.test :refer :all]
            [caesarhu.math.polynomial :refer :all]))

(deftest quadratic-root-test
  (testing "quadratic-root test"
    (is (= #{1} (quadratic-root 1 -2 1)))
    (is (= #{1 2} (quadratic-root 1 -3 2)))
    (is (nil? (quadratic-root 1 2 2)))))