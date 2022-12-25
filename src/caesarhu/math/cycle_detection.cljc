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