(ns caesarhu.math.chinese-remainder)

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(abs b) 0 1]
        (zero? b) [(abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (abs b)
                     r0 (abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (*' q s)) s
                           (- t0 (*' q t)) t
                           (- r0 (*' q r)) r))))))

(defn chinese_remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply *' n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+' sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
    [sum-prod prod]))

(defn crt
  [n a]
  (let [[sum-prod prod] (chinese_remainder n a)]
    (mod sum-prod prod)))

(defn crt-seq
  [n a]
  (let [[sum-prod prod] (chinese_remainder n a)]
    (iterate #(+' prod %) (mod sum-prod prod))))
