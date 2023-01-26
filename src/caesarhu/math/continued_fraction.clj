(ns caesarhu.math.continued-fraction
  (:require [clojure.math.numeric-tower :refer [sqrt floor]]))

(defn continued-fraction
  [x]
  (if (integer? x) [x]
      (let [x (rationalize x)]
        (loop [[n d] [(numerator x) (denominator x)]
               result []]
          (if (zero? (mod d n))
            result
            (recur [d (mod n d)]
                   (conj result (->> (/ n d) floor long))))))))

(defn continued-fraction-periodic
  ([p q d s]
   (let [[p q d s] (map long (if (neg? q)
                               [(- p) (- q) d (- s)]
                               [p q d s]))
         sd (sqrt d)]
     (assert (not (neg? d)) "expected non-negative for `d`")
     (assert (not (zero? q)) "The denominator cannot be 0.")
     (if (integer? sd)
       [(continued-fraction (/ (+' p (*' s sd)) q)) []]
       (let [d (*' d s s)
             sd (*' sd s)
             [d sd p q] (map long (if (zero? (mod (-' d (*' p p)) q))
                                    [(*' d q q) (*' sd q) (*' p q) (*' q q)]
                                    [d sd p q]))]
         (loop [[p q] [p q]
                terms []
                pq (sorted-set)]
           (if (pq [p q])
             terms
             (let [n (quot (+' p sd) q)
                   p (-' (*' n q) p)]
               (recur [p (quot (-' d (*' p p)) q)]
                      (conj terms n)
                      (conj pq [p q])))))))))
  ([p q d]
   (continued-fraction-periodic p q d 1)))

(comment
  (->> (apply sorted-set (range 10))
       (split-with #(not= 5 %)))
  (split-at 5 (range 10))
  (continued-fraction-periodic 4 3 49)
  )