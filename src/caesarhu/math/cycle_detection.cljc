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
   if s is cycled, returns [(head...) (cycled-seq)], else returns nil."
  ([s]
   (floyd-detection-seq s =))
  ([s comparator]
   (let [tortoise (drop 1 s)
         hare (->> (take-nth 2 s) (drop 1))]
     (when-let [[_ _ i] (->> (map vector tortoise hare (range))
                            (some #(and (apply comparator (take 2 %)) %)))]
       (let [[start _ j] (->> (map vector s (drop i tortoise) (range))
                              (some #(and (apply comparator (take 2 %)) %)))
             [head cycled-seq] (split-at j s)]
         [head (cons start (take-while #(not (comparator start %)) (rest cycled-seq)))])))))

(comment
  (defn tt
    [n]
    (concat (repeatedly n  #(rand-int 100000)) (cycle (range 7))))
  (floyd-detection-seq (tt 5))
  )