(ns caesarhu.math.math-tools
  (:require [clojure.math.numeric-tower :refer :all :exclude [abs]]))

(defn square?
  "Is n a square number?"
  [n]
  (-> n exact-integer-sqrt last zero?))

(defn divmod
  "Return quotient and remainder."
  [n d]
  [(quot n d) (mod n d)])

(defn digits
  "Transform a number to digits sequence."
  ([n base]
   (if (zero? n) [0]
       (loop [n (abs n)
              result []]
         (let [q (quot n base)]
           (if (zero? n)
             result
             (recur q (cons (int (- n (* q base))) result)))))))
  ([n]
   (digits n 10)))

(defn digits->number
  "Transform a digits sequence to a number."
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
  "Lazy factorials in Clojure."
  [n]
  (if (nat-int? n)
    (if (< n 2) 1
        (reduce *' (range 2 (inc n))))
    (throw (Exception. "factorial argument must be nonnegative int."))))

(defn- -binomial
  "n and k must be positive."
  [n k]
  (quot (factorial n)
        (*' (factorial k) (factorial (- n k)))))

(defn binomial
  "The binomial distribution."
  [n k]
  (if (neg? n)
    (let [b (-binomial (- k n 1) k)]
      (if (odd? n) (- b) b))
    (-binomial n k)))

(defn gcd*
  "(gcd a b ...) returns the greatest common divisor of (a b ...)"
  [& xs]
  (reduce (fn [acc x]
            (if (= acc 1) 1
                (gcd acc x)))
          xs))

(defn lcm*
  "(lcm a b ...) returns the least common multiple of (a b ...)"
  [& xs]
  (reduce (fn [acc x]
            (lcm acc x))
          xs))

(defn palindrome?
  "Is n a palindromic number?"
  [n]
  (let [ds (digits n)]
    (= ds (reverse ds))))

(defn sqrt-continued-fraction
  "find the continued fraction of sqrt of n."
  [n]
  (let [[a0 r] (exact-integer-sqrt n)]
    (if (zero? r)
      [a0]
      (loop [m 0, d 1, a a0, acc [a0]]
        (if (= a (* 2 a0))
          acc
          (let [m (- (* d a) m), d (/ (- n (* m m)) d), a (quot (+ a0 m) d)]
            (recur m d a (conj acc a))))))))

(defn coprime?
  [x y]
  (= 1 (gcd x y)))

; https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples
(def triangle-pattern
  [[[2 1 -1]
    [-2 2 2]
    [-2 1 3]]
   [[2 1 1]
    [2 -2 2]
    [2 -1 3]]
   [[2 -1 1]
    [2 2 2]
    [2 1 3]]])

(defn next-pythagorean-triplet
  "Generate next pythagorean triplet from base(3,4,5) triplet."
  [[^long a, ^long b, ^long c]]
  (map (fn [s] (map #(->> (map * % [a b c]) (apply +)) s)) triangle-pattern))

(defn pythagorean-triplet
  "Generate lazy pythagorean triplet sequence."
  ([]
   (->> (iterate #(mapcat next-pythagorean-triplet %) (list (list 3 4 5)))
        (apply concat)))
  ([f]
   (loop [base (list (list 3 4 5))
          result (list (list 3 4 5))]
     (if (empty? base)
       result
       (let [new-triplets (->> (next-pythagorean-triplet (first base))
                               (filter f))]
         (recur (apply conj (rest base) new-triplets) (apply conj result new-triplets)))))))

(comment
  (pythagorean-triplet)
  (time (->> (pythagorean-triplet #(<= (last %) 1000000000))
             count))
  )
