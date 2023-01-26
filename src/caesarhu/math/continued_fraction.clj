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
   (let [[p q d s] (if (neg? q)
                     [(- p) (- q) d (- s)]
                     [p q d s])
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
                pq {}
                i 0]
           (if (pq [p q])
             (split-at (pq [p q]) terms)
             (let [n (let [temp (/ (+' p sd) q)]
                       (if (neg? temp)
                         (-> temp floor long)
                         (quot (+' p sd) q)))
                   new-p (-' (*' n q) p)]
               (recur [new-p (quot (-' d (*' new-p new-p)) q)]
                      (conj terms n)
                      (assoc pq [p q] i)
                      (inc i)))))))))
  ([p q d]
   (continued-fraction-periodic p q d 1))
  ([d]
   (continued-fraction-periodic 0 1 d 1)))

(comment
  (continued-fraction-periodic 3 2 7)
  )