(ns user
  (:require [caesarhu.math.math-tools :refer :all]
            [caesarhu.math.cycle-detection :as cycle]))

(defn next-digit
  [[n m]]
  (when (not (zero? n))
    [(mod (* n 10) m) m]))

(comment
  (cycle/floyd-detection next-digit [1 983])
  )