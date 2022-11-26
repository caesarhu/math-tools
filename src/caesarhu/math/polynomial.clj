(ns caesarhu.math.polynomial
  (:require [clojure.math.numeric-tower :as math]))

(defn quadratic-discriminant
  "discriminant of quadratic polynomial."
  [a b c]
  (- (* b b) (* 4 a c)))

(defn quadratic
  "generates a quadratic polynomial function."
  [a b c]
  (fn [x]
    (+ (* a x x)
       (* b x)
       c)))

(defn quadratic-root
  "find a quadratic polynomial root."
  [a b c]
  (when-not (neg? (quadratic-discriminant a b c))
    (let [sqr (math/sqrt (quadratic-discriminant a b c))
          -b (- b)
          deno (* 2 a)]
      (->> [(+ -b sqr) (- -b sqr)]
           (map #(/ % deno))
           set))))