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

(defn neville
  "Neville's algorithm for polynomial interpolation.
   xs: x values vector
   ys: y values vector
   n : x^n to evaluate
   x : x value to evaluate"
  [xs ys n x]
  ((memoize (fn p [i j]
              (if (= i j) (ys i)
                  (/ (- (*' (- x (xs j)) (p i (dec j)))
                        (*' (- x (xs i)) (p (inc i) j)))
                     (- (xs i) (xs j))))))
   0 n))
