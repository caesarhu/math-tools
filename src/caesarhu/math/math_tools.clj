(ns caesarhu.math.math-tools
  (:require [clojure.math.numeric-tower :refer :all]))

(defn square?
  "Is n a square number?"
  [n]
  (-> n exact-integer-sqrt last zero?))

(defn digits
  ([n base]
   (if (zero? n) [0]
       (loop [n (abs n) result []]
         (let [q (quot n base)]
           (if (zero? n) result (recur q (cons (- n (* q base)) result)))))))
  ([n]
   (digits n 10)))

(defn digits->number
  ([xs base]
   (reduce (fn [acc x]
             (+' (*' acc base) x))
           0 xs))
  ([xs]
   (digits->number xs 10)))

(defn power-mod
  " b^e mod m (using Java which solves some cases the pure clojure method has to be modified to tackle--i.e. with large b & e and 
    calculation simplications when gcd(b, m) == 1 and gcd(e, m) == 1) "
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defn factorial
  [n]
  (cond
    (neg? n) (throw (Exception. "factorial argument must be nonnegative."))
    (< n 2) 1
    :else (reduce *' (range 2 (inc n)))))

(defn- -binomial
  "n and k must be positive."
  [n k]
  (quot (factorial n)
        (*' (factorial k) (factorial (- n k)))))

(defn binomial
  [n k]
  (if (neg? n)
    (let [b (-binomial (- k n 1) k)]
      (if (odd? n) (- b) b))
    (-binomial n k)))

(defn gcd*
  [& xs]
  (reduce (fn [acc x]
            (if (= acc 1) 1
                (gcd acc x)))
          xs))

(defn lcm*
  [& xs]
  (reduce (fn [acc x]
            (lcm acc x))
          xs))

(defn palindrome?
  [n]
  (let [ds (digits n)]
    (= ds (reverse ds))))