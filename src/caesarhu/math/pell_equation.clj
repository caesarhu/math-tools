(ns caesarhu.math.pell-equation
  (:require [caesarhu.math.math-tools :as tools]))

(defn find-fundamental-solution
  "find the fundamental solution of pell's equation,  x^2 - dy^2 = 1 or -1"
  ([d r]
   (when (and (integer? r)
              (integer? d)
              (pos? d)
              (not (or (zero? d) (zero? r) (tools/square? d))))
     (let [cf (tools/sqrt-continued-fraction d)
           as (lazy-cat cf (cycle (rest cf)))
           continued-fractions (fn []
                                 (loop [h2 0, h1 1
                                        k2 1, k1 0
                                        as as, n 0]
                                   (if (and (>= n 1) (= r (-' (*' h1 h1) (*' d k1 k1))))
                                     [h1 k1]
                                     (recur h1 (+' (*' (first as) h1) h2)
                                            k1 (+' (*' (first as) k1) k2)
                                            (rest as) (inc n)))))]
       (cond
         (= 1 r) (continued-fractions)
         (= -1 r) (when (even? (count cf))
                    (continued-fractions))))))
  ([d]
   (find-fundamental-solution d 1)))

(defn pell-solutions
  "find the solutions of pell's equation,  x^2 - dy^2 = 1 or -1"
  ([d r]
   (when-let [[x y] (find-fundamental-solution d r)]
     (let [prod (fn [[xk yk]]
                  [(+' (*' x xk) (*' d y yk))
                   (+' (*' x yk) (*' y xk))])
           solutions (iterate prod [x y])]
       (cond
         (= 1 r) solutions
         (= -1 r) (take-nth 2 solutions)))))
  ([d]
   (pell-solutions d 1)))

(comment
  (pell-solutions 3 -1)
  )