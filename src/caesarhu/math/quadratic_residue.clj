(ns caesarhu.math.quadratic-residue
  (:require [caesarhu.math.math-tools :refer [power-mod digits]]))

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

(comment
  (sqrt-mod-bf 64 120)
  (cipolla 3 37)
  )