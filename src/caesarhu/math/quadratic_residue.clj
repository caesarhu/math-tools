(ns caesarhu.math.quadratic-residue
  (:require [caesarhu.math.math-tools :refer [power-mod digits]]))

(defn find-first
  " Finds first element of collection that satisifies predicate function pred "
  [pred coll]
  (first (filter pred coll)))

(defn sqrt-mod-map
  [p]
  (->> (for [i (range (inc (quot p 2)))
             :let [m (mod (*' i i) p)]]
         {m [i]})
       (apply merge-with concat)))

(defn sqrt-mod-bf
  [n p]
  (let [m (sqrt-mod-map p)
        n (mod n p)]
    (m n)))

;-------------------------------------------------------------------------------------------------

(defn legendre [a p]
  (power-mod a (quot (dec p) 2) p))

(defn cipolla-*
  [[a b] [c d] w p]
  (if (= 1 p)
    [(+' (*' a c) (*' b d w))
     (+' (*' a d) (*' b c))]
    [(mod (+' (*' a c) (*' b d w)) p)
     (mod (+' (*' a d) (*' b c)) p)]))

(defn cipolla-power
  [[a b :as base] w p e]
  (let [exp (rest (digits e 2))]
    (reduce (fn [acc d]
              (let [acc2 (cipolla-* acc acc w p)]
                (if (zero? d)
                  acc2
                  (cipolla-* acc2 base w p))))
            base exp)))

(defn cipolla
  [n p]
  (assert (> p 2) "p must > 2")
  (let [n (mod n p)
        phi (dec p)
        get-w (fn [x] (mod (-' (*' x x) n) p))]
    (cond
      (zero? n) [0]
      (= 1 n) [1 phi]
      (= phi (legendre n p)) nil
      (= 3 (mod p 4)) (let [m (power-mod n (/ (inc p) 4) p)]
                        (sort [m (-' p m)]))
      :else (let [[w a] (->> (repeatedly #(inc (rand-int (dec p))))
                             (map #(vector (get-w %) %))
                             (filter #(= phi (legendre (first %) p)))
                             first)
                  [result _] (cipolla-power [a 1] w p (/ (inc p) 2))]
              (sort [result (-' p result)])))))

(defn tonelli [n p]
  " Following Wikipedia https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm "
  (when (= (legendre n p) 1)
    (loop [q (dec p)                                                  ; Step 1 in Wikipedia
           s 0]
      (if (zero? (rem q 2))
        (recur (quot q 2) (inc s))
        (if (= s 1)
          (power-mod n (quot (inc p) 4) p)
          (let [z (find-first #(= (dec p) (legendre % p)) (range 2 p))] ; Step 2 in Wikipedia
            (loop [M s
                   c (power-mod z q p)
                   t (power-mod n q p)
                   R (power-mod n (quot (inc q) 2) p)]
              (if (= t 1)
                R
                (let [i (long (find-first #(= 1 (power-mod t (bit-shift-left 1 %) p)) (range 1 M))) ; Step 3
                      b (power-mod c (bit-shift-left 1 (- M i 1)) p)
                      M i
                      c (power-mod b 2 p)
                      t (rem (* t c) p)
                      R (rem (* R b) p)]
                  (recur M c t R))))))))))

(comment
  (cipolla 3 37)
  (tonelli 10 37)
  )
