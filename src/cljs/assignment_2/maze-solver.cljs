(def grid-rb [[{:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
               {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
               {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 1}
               {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}]
              [{:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
               {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
               {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}
               {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}
               {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
              [{:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
               {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 0}
               {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}
               {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
              [{:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
               {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
               {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
               {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
               {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
              [{:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
               {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
               {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 0}
               {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}]])

(def grid-bt [[{:west 0, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 0, :distance -1, :north 1}
               {:west 0, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 0, :distance -1, :north 1}]
              [{:west 0, :south 0, :visited 0, :east 0, :distance -1, :north 1}
               {:west 0, :south 0, :visited 0, :east 0, :distance -1, :north 1}
               {:west 0, :south 1, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 1, :visited 0, :east 0, :distance -1, :north 1}]
              [{:west 0, :south 1, :visited 0, :east 0, :distance -1, :north 1}
               {:west 0, :south 1, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 0, :distance -1, :north 1}
               {:west 0, :south 0, :visited 0, :east 0, :distance -1, :north 1}
               {:west 0, :south 1, :visited 0, :east 0, :distance -1, :north 1}]
              [{:west 0, :south 1, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 1, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 1, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 1, :visited 0, :east 0, :distance -1, :north 1}]
              [{:west 0, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 0, :visited 0, :east 1, :distance -1, :north 0}
               {:west 1, :south 1, :visited 0, :east 0, :distance -1, :north 0}]])

(def grid-rb-2 [[{:west 0, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}]
                [{:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
                [{:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
                [{:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
                [{:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
                [{:west 0, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}]
                [{:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}]
                [{:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
                [{:west 0, :south 1, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 1}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 1}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 1}]
                [{:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}
                 {:west 0, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 0, :visited 1, :east 1, :distance -1, :north 0}
                 {:west 1, :south 1, :visited 1, :east 0, :distance -1, :north 0}]])

(defn square "Returns the squared value of a given number"
  [x]
  (* x x))

(defn find-neighbour [direction x y]
  (let [direction-name (get direction 0)]
    (cond
      (= direction-name :north) (hash-map :x (+ x 1), :y y)
      (= direction-name :east) (hash-map :x x, :y (+ y 1))
      (= direction-name :south) (hash-map :x (+ x -1), :y y)
      (= direction-name :west) (hash-map :x x, :y (+ y -1)))))

(defn neighbours [x y]
  (map #(find-neighbour % x y) (filter (fn [[k v]] (= v 1)) (dissoc (get-in grid-rb-2 [x y]) :visited :distance))))

(defn euclidean-distance [x1 y1 x2 y2]
  (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))

(defn lowest-f-score [list]
  (first (sort-by :f list)))

(defn is-in-list? [item list]
  (some #(and (= (:x item) (:x %)) (= (:y item) (:y %))) list))

(def is-not-in-list? (complement is-in-list?))

(defn calc-neighbours [neighbour current closed-list end-x end-y]
  (if (is-in-list? neighbour closed-list)
    nil
    (let [g (+ (:g current) 1) h (euclidean-distance (:x neighbour) (:y neighbour) end-x end-y)]
      (assoc neighbour :g g :f (+ g h) :p (dissoc current :p)))))

(defn clean-dupes [open-list]
  (let [sorted-open-list (sort-by (juxt :x :y :f) open-list)]
    (let [first (first sorted-open-list) second (second sorted-open-list)]
      (if (and (= (:x first) (:x second)) (= (:y first) (:y second)))
        (remove #(= % second) open-list)
        open-list))))

(defn a* [start-x start-y end-x end-y]
  (let [start-open-list [{:x start-x :y start-y :g 0 :f (euclidean-distance start-x start-y end-x end-y)}]]
    (loop [open-list start-open-list closed-list []]
      (let [current (lowest-f-score open-list)
            new-open-list (vec (remove #(= % current) open-list))
            new-closed-list (conj closed-list current)]
        (if (and (= (:x current) end-x) (= (:y current) end-y))
           (conj closed-list current)
           (recur (vec (clean-dupes (concat new-open-list (keep #(calc-neighbours % current new-closed-list end-x end-y) (neighbours (:x current) (:y current))))))
                  new-closed-list))))))
