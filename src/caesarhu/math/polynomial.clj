(ns caesarhu.math.polynomial
  (:require [clojure.math.numeric-tower :as math]))

(defn quadratic
  [a b c]
  (fn [x]
    (+ (* a x x)
       (* b x)
       c)))

(defn quadratic-root
  [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (if (neg? discriminant)
      []
      (let [sqr (math/sqrt discriminant)
            deno (* 2 a)]
        [(/ (+ (- b) sqr) deno) (/ (- (- b) sqr) deno)]))))

(defn quadratic-root-pred?
  [pred a b c]
  (some pred (quadratic-root a b c)))