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
  (let [d (quadratic-discriminant a b c)]
    (when-not (neg? d)
      (let [sqr (math/sqrt d)
            deno (* 2 a)]
        (if (zero? d)
          [(/ (- b) deno)]
          [(/ (+ (- b) sqr) deno) (/ (- (- b) sqr) deno)])))))
