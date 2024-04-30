(ns caesarhu.math.cycle-detection)

(defn- double-fn
  "Double function call from cycle detection."
  [f]
  (fn [x]
    (when-let [x1 (and x (f x))]
      (f x1))))

(defn floyd-cycle?
  "Floyd Cycle Detection Algorithm.
   f is a function to change state, x0 is the initial state.
   if not cycled, returns nil, else returns (hare counter)."
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
  "Floyd Cycle Detection Algorithm.
   f is a function to change state, x0 is the initial state.
   if not cycled, returns nil, else returns (start length)."
  [f x0]
  (let [f2 (double-fn f)]
    (when-let [meet (floyd-cycle? f x0)]
      (let [[hare _] meet]
        (if (= hare x0)
          meet
          (let [start (loop [tortoise x0
                             hare hare]
                        (if (= tortoise hare) hare
                            (recur (f tortoise) (f hare))))
                length (loop [tortoise (f start)
                              hare (f2 start)
                              counter 1]
                         (if (= tortoise hare) counter
                             (recur (f tortoise) (f2 hare) (inc counter))))]
            [start length]))))))

(defn floyd-detection-seq
  "Floyd Cycle Detection Algorithm.
   if s is cycled, returns [hare counter], else returns nil."
  [s]
  (let [next2 (fn [ss]
                (when (some? (first ss))
                  (when (some? (first (next ss)))
                    (nnext ss))))]
    (loop [s1 (next s)
           s2 (next2 s)
           counter 1]
      (let [tortoise (first s1)
            hare (first s2)]
        (cond
          (nil? hare) nil
          (= tortoise hare) [hare counter]
          :else (recur (next s1) (next2 s2) (inc counter)))))))
