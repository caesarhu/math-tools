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
  (when-let [meet (floyd-cycle? f x0)]
    (let [[hare _] meet]
      (if (= hare x0)
        meet
        (let [start (loop [tortoise x0
                           hare hare]
                      (if (= tortoise hare) hare
                          (recur (f tortoise) (f hare))))
              length (loop [hare (f start)
                            counter 1]
                       (if (= start hare) counter
                           (recur (f hare) (inc counter))))]
          [start length])))))

(defn floyd-detection-seq
  "Floyd Cycle Detection Algorithm.
   if s is cycled, returns [hare counter], else returns nil."
  [s]
  (let [next2 (fn [ss]
                (when (and ss (some? (first ss)) (some? (first (next ss))))
                  (nnext ss)))]
    (loop [[tortoise & _ :as s1] (next s)
           [hare & _ :as s2] (next2 s)
           counter 1]
       (cond
         (nil? hare) nil
         (= tortoise hare) [hare counter]
         :else (recur (next s1) (next2 s2) (inc counter))))))

(defn brent-cycle?
  "Brent Cycle Detection Algorithm.
   f is a function to change state, x0 is the initial state.
   if not cycled, returns nil, else returns cycle length."
  [f x0]
  (loop [max-steps 1
         tortoise x0]
    (let [[hare steps] (loop [steps 1
                              hare (f tortoise)]
                         (cond
                           (nil? hare) nil
                           (= tortoise hare) [hare steps]
                           (>= steps max-steps) [hare nil]
                           :else (recur (inc steps) (f hare))))]
      (cond
        (nil? hare) nil
        steps steps
        :else (recur (* 2 max-steps) hare)))))

(defn brent-detect
  "Brent Cycle Detection Algorithm.
   f is a function to change state, x0 is the initial state.
   if not cycled, returns nil, else returns (start length)."
  [f x0]
  (when-let [steps (brent-cycle? f x0)]
    (loop [tortoise x0
           hare (reduce (fn [xn n] (f xn)) x0 (range steps))]
      (if (= tortoise hare)
        [hare steps]
        (recur (f tortoise) (f hare))))))

(defn brent-detect-seq
  "Brent Cycle Detection Algorithm.
   if s is cycled, returns cycle length, else returns nil."
  [s]
  (loop [[tortoise & _ :as s] s
         max-steps 1]
    (let [[hare steps] (loop [[hare & _ :as seq] (next s)
                              steps 1]
                         (cond
                           (nil? hare) nil
                           (= tortoise hare) [hare steps]
                           (>= steps max-steps) [seq nil]
                           :else (recur (next seq) (inc steps))))]
      (cond
        (nil? hare) nil
        steps steps
        :else (recur hare (* 2 max-steps))))))
