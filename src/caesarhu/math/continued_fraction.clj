(ns caesarhu.math.continued-fraction
  (:require [clojure.math.numeric-tower :refer [sqrt floor ceil exact-integer-sqrt]]
            [caesarhu.math.cycle-detection :refer [floyd-detection-seq]]))

(defn continued-fraction
  [x]
  (if (integer? x) [x]
      (let [x (rationalize x)]
        (loop [[n d] [(numerator x) (denominator x)]
               result []]
          (let [nn (mod n d)
                an (-> (/ n d) floor bigint)]
            (if (zero? nn)
              (conj result n)
              (recur [d nn]
                     (conj result an))))))))

(defn periodic-seq
  [p q d sd]
  (let [calc-n (fn [p q] (-> (quot (+' p sd) q) long))
        periodic-next (fn [[_ [p q]]]
                        (let [n (calc-n p q)
                              new-p (-' (*' n q) p)
                              new-q (quot (-' d (*' new-p new-p)) q)]
                          [n [new-p new-q]]))]
    (->> (iterate periodic-next [0 [p q]])
         rest)))

(defn continued-fraction-periodic
  ([d]
   (continued-fraction-periodic 0 1 d))
  ([p q]
   (continued-fraction-periodic p q 0))
  ([p q d]
   (continued-fraction-periodic p q d 1))
  ([p q d s]
   (assert (nat-int? d) "expected non-negative for `d`")
   (assert (not= q 0) "The denominator cannot be 0.")
   (let [sd (sqrt d)
         n (/ (+' p (*' s sd)) q)]
     (cond
       (zero? (-> (exact-integer-sqrt d) last)) [(continued-fraction (/ (+' p (sqrt d)) q)) []]
       (neg? q) (recur (- p) (- q) d (- s))
       (neg? n) (let [x (-> (/ (-' (+' p sd)) q) ceil long)
                      new-p (+' p (*' x q))
                      start (-> n floor long)
                      [head tail] (continued-fraction-periodic new-p q d s)]
                  [(cons start (rest head)) tail])
       :else (let [temp-d (*' d s s)
                   sd (*' sd s)
                   [p q d sd] (if (pos-int? (mod (-' temp-d (*' p p)) q))
                             [(*' p q) (*' q q) (*' temp-d q q) (*' sd q)]
                             [p q temp-d sd])]
               (->> (periodic-seq p q d sd)
                    floyd-detection-seq
                    (map (fn [v] (map first v)))
                    vec))))))

(comment
  (continued-fraction 3)
  (continued-fraction-periodic 10)
  )