(ns caesarhu.math.pell-equation
  (:require [caesarhu.math.math-tools :as tools]
            [clojure.math.numeric-tower :refer [floor exact-integer-sqrt]]
            [caesarhu.math.quadratic-residue :refer [sqrt-mod-bf]]
            [caesarhu.math.continued-fraction :refer [continued-fraction-periodic]]))

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
  (let [sqrt-D (first (exact-integer-sqrt D))
        calc-ai (fn [p q]
                  (->> (/ (+' p sqrt-D) q) floor))
        calc-abg (fn [ai [x1 x2]]
                   (let [xi (+' (*' ai x1) x2)]
                     [xi x1]))
        calc-pq (fn [ai p q]
                  (let [pi (-' (*' ai q) p)
                        qi (/ (-' D (*' pi pi)) q)]
                    [[pi p] [qi q]]))
        init [[1 0]
              [0 1]
              [Q_0 (- P_0)]
              [P_0 0]
              [Q_0 0]
              [0 0]]
        get-args (fn [v]
                   (let [[[A_i_1 A_i_2]
                          [B_i_1 B_i_2]
                          [G_i_1 G_i_2]
                          [P_i_1 P_i_2]
                          [Q_i_1 Q_i_2]
                          [a_i_1 a_i_2]] v]
                     [P_i_2 Q_i_2 a_i_1 A_i_1 B_i_1 G_i_1]))
        next (fn [v]
               (let [[p q] (->> (drop 3 v) (take 2) (map first))
                     ai (calc-ai p q)]
                 (concat (map #(calc-abg ai %) (take 3 v))
                         (calc-pq ai p q)
                         [[ai (->> v last first)]])))]
    (->> (iterate next init)
         (drop 1)
         (map get-args))))

(defn square-divisors
  [N]
  (for [f (iterate inc 1)
        :let [f2 (*' f f)]
        :while (<= f2 (abs N))
        :let [[m r] (tools/divmod N f2)]
        :when (zero? r)]
    [m f]))

(defn special-diop-DN
  [D N]
  (let [sqrt-D (first (exact-integer-sqrt D))
        fs (square-divisors N)]
    (loop [P 0 Q 1
           G0 0 G1 1
           B0 1 B1 0
           solutions []
           counter 0]
      (let [a (quot (+' P sqrt-D) Q)
            P (-' (*' a Q) P)
            Q (quot (-' D (*' P P)) Q)
            G2 (+' (*' a G1) G0)
            B2 (+' (*' a B1) B0)
            new-solutions (for [[n f] fs
                                :when (= n (-' (*' G2 G2) (*' D B2 B2)))]
                            [(*' f G2) (*' f B2)])]
        (if (and (= 1 Q) (odd? counter))
          solutions
          (recur P Q G1 G2 B1 B2 (concat solutions new-solutions) (inc counter)))))))

(defn diop-DN-1
  [D N]
  (let [sd2 (-> (exact-integer-sqrt D) first (* 2))
        pqa (PQa 0 1 D)
        length (->> (take-while #(< (nth % 2) sd2) pqa) count)
        get-xy (fn [v] [(nth v 5) (nth v 4)])
        nth-pqa (fn [n] (->> n (nth pqa) get-xy list))]
    (if (odd? length)
      (if (pos-int? N)
        (->> (* length 2) dec nth-pqa)
        (->> length dec nth-pqa))
      (when (pos-int? N)
        (->> length dec nth-pqa)))))

(defn generalized-DN
  [D N]
  (->> (for [[m f] (square-divisors N) ; 要改為較有效率的square-divisors
             zz (sqrt-mod-bf D (abs m)) ; 要改為較有效率的sqrt-mod
             z (seq (set [zz (- zz)]))]
         (let [length (->> (continued-fraction-periodic z (abs m) D) (map count) (apply +))
               pqa (->> (PQa z (abs m) D) (take length))
               get-xy (fn [v] [(nth v 5) (nth v 4)])]
           (when (some #(= 1 (abs (nth % 1))) pqa)
             (let [[r s] (-> (some #(and (= 1 (abs (nth (last %) 1))) (first %)) (partition 2 1 pqa))
                             get-xy)]
               (if (= m (-' (*' r r) (*' D s s)))
                 [(*' f r) (*' f s)]
                 (when-let [[u v] (->> (diop-DN-1 D -1) first)]
                   [(*' f (+' (*' r u) (*' v s D)))
                    (*' f (+' (*' r v) (*' s u)))]))))))
       (remove nil?)))

(defn diop-DN
  [D N]
  (cond
    (= 1 (abs N)) (diop-DN-1 D N)
    (< 1 (*' N N) D) (special-diop-DN D N)
    :else (generalized-DN D N)))

(comment
  (tools/sqrt-continued-fraction 61)
  (diop-DN 5 -124)
  )
