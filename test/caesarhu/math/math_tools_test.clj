(ns caesarhu.math.math-tools-test
  (:require [clojure.test :refer :all]
            [caesarhu.math.math-tools :refer :all]))

(deftest square-test
  (testing "square?"
    (let [x (rand-int 1000000)]
      (is (true? (square? (* x x)))))))
