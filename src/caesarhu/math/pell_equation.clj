(ns caesarhu.math.pell-equation
  (:require [caesarhu.math.math-tools :as tools]
            [clojure.math.numeric-tower :refer [floor sqrt exact-integer-sqrt]]))

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

(defn PQa
  [P_0, Q_0, D]
  (let [sqrt-D (sqrt D)
        init [[1 0]
              [0 1]
              [Q_0 (- P_0)]
              [P_0 0]
              [Q_0 0]
              [(long (/ (+ P_0 sqrt-D) Q_0)) 0]]
        get-args (fn [v]
                   (let [[[A_i_1 A_i_2]
                          [B_i_1 B_i_2]
                          [G_i_1 G_i_2]
                          [P_i_1 P_i_2]
                          [Q_i_1 Q_i_2]
                          [a_i_1 a_i_2]] v]
                     [P_i_2 Q_i_2 a_i_1 A_i_1 B_i_1 G_i_1]))
        next (fn [[A_i_1 A_i_2]
                  [B_i_1 B_i_2]
                  [G_i_1 G_i_2]
                  [P_i_1 P_i_2]
                  [Q_i_1 Q_i_2]
                  [a_i_1 a_i_2]]
               (let [a_i (long (/ (+ P_i_1 sqrt-D) Q_i_1))
                     A_i (+' (*' a_i A_i_1) A_i_2)
                     B_i (+' (*' a_i B_i_1) B_i_2)
                     G_i (+' (*' a_i G_i_1) G_i_2)
                     P_i (-' (*' a_i Q_i_1) P_i_1)
                     Q_i (/ (-' D (*' P_i P_i)) Q_i_1)]
                 [[A_i A_i_1]
                  [B_i B_i_1]
                  [G_i G_i_1]
                  [P_i P_i_1]
                  [Q_i Q_i_1]
                  [a_i a_i_1]]))]
    (->> (iterate #(apply next %) init)
         (drop 1)
         (map get-args))))

(defn special_diop_DN
  [D N]
  (let [sqrt-D (sqrt D)
        F (for [f (iterate inc 1)
                :let [f2 (*' f f)]
                :while (<= f2 (abs N))
                :let [[n r] (tools/divmod N f2)]
                :when (zero? r)]
            [n f])]
    (loop [P 0 Q 1
           G0 0 G1 1
           B0 1 B1 0
           solutions []
           counter 0]
      (let [a (long (/ (+ P sqrt-D) Q))
            P (-' (*' a Q) P)
            Q (quot (-' D (*' P P)) Q)
            G2 (+' (*' a G1) G0)
            B2 (+' (*' a B1) B0)
            new-solutions (for [[n f] F
                                :when (= n (-' (*' G2 G2) (*' D B2 B2)))]
                            [(*' f G2) (*' f B2)])]
        (if (and (= 1 Q) (odd? counter))
          solutions
          (recur P Q G1 G2 B1 B2 (concat solutions new-solutions) (inc counter)))))))

(defn diop-DN
  [D N]
  (let [get-xy (fn [v] [(nth v 5) (nth v 4)])]
    (cond
      (= 1 (abs N)) (let [sd2 (-> (exact-integer-sqrt D) first (* 2))
                          pqa (PQa 0 1 D)
                          length (->> (take-while #(< (nth % 2) sd2) pqa) count)]
                      (if (odd? length)
                        (if (pos-int? N)
                          (->> (* length 2) dec (nth pqa) get-xy list)
                          (->> length dec (nth pqa) get-xy list))
                        (when (pos-int? N)
                          (->> length dec (nth pqa) get-xy list))))
      (< 1 (*' N N) D) (special_diop_DN D N))))

(comment
  
  )
