(ns caesarhu.math.cycle-detection)

(defn double-fn
  [f]
  (fn [x]
    (when-let [x1 (f x)]
      (f x1))))

(defn floyd-cycle?
  "f: function for change state
   x0: start state"
  [f x0]
  (let [f2 (double-fn f)]
    (loop [tortoise (f x0)
           hare (f2 x0)
           counter 1]
      (cond
        (nil? hare) nil
        (= tortoise hare) [hare counter]
        :else (recur (f tortoise) (f2 hare) (inc counter))))))

(defn floyd-detection
  [f x0]
  (let [f2 (double-fn f)]
    (when-let [meet (floyd-cycle? f x0)]
      (let [[hare _] meet]
        (if (= hare x0)
          meet
          (let [start (loop [tortoise x0
                             hare meet]
                        (if (= tortoise hare) hare
                            (recur (f tortoise) (f hare))))
                length (loop [tortoise (f x0)
                              hare (f2 x0)
                              counter 1]
                         (if (= tortoise hare) counter
                             (recur (f tortoise) (f2 hare) (inc counter))))]
            [start length]))))))