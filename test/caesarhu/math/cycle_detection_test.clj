(ns caesarhu.math.cycle-detection-test
  (:require [clojure.test :refer :all]
            [caesarhu.math.cycle-detection :refer [floyd-cycle? floyd-detection]]))

(defn next-digit
  [[n m]]
  (when (not (zero? n))
    [(mod (* n 10) m) m]))

(deftest floyd-test
  (testing "floyd-cycle? test"
    (is (= [[1 983] 982] (floyd-cycle? next-digit [1 983])))
    (is (= [[1 983] 982] (floyd-detection next-digit [1 983])))))

(comment
  (floyd-detection next-digit [20 983])
  )