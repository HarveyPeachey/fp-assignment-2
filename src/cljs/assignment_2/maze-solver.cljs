(ns assignment-2.maze-solver "Maze Solver")

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

(defn neighbours [x y grid]
  (map #(find-neighbour % x y) (filter (fn [[k v]] (= v 1)) (dissoc (get-in grid [x y]) :visited :distance))))

(defn euclidean-distance [x1 y1 x2 y2]
  (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))

(defn manhattan-distance [x1 y1 x2 y2]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn lowest-f-score [list]
  (first (sort-by :f list)))

(defn is-in-list? [item list]
  (some #(and (= (:x item) (:x %)) (= (:y item) (:y %))) list))

(defn calc-neighbours [neighbour current closed-list end-x end-y]
  (if (is-in-list? neighbour closed-list)
    nil
    (let [g (+ (:g current) 1) h (manhattan-distance (:x neighbour) (:y neighbour) end-x end-y)]
      (assoc neighbour :g g :f (+ g h) :p (- (count closed-list) 1)))))

(defn clean-dupes [open-list]
  (let [sorted-open-list (sort-by (juxt :x :y :f) open-list)]
    (let [first (first sorted-open-list) second (second sorted-open-list)]
      (if (and (= (:x first) (:x second)) (= (:y first) (:y second)))
        (remove #(= % second) open-list)
        open-list))))

(defn is-in-grid? [start-x start-y end-x end-y grid]
  (let [no-of-rows (count grid)
        no-of-cols (count (first grid))]
    (and (<= 0 start-x (- no-of-rows 1)) (<= 0 start-y (- no-of-cols 1)) (<= 0 end-x (- no-of-rows 1)) (<= 0 end-y (- no-of-cols 1)))))

(defn a* [start-x start-y end-x end-y grid]
  (let [start-open-list [{:x start-x :y start-y :g 0 :f (manhattan-distance start-x start-y end-x end-y) :p -1}]]
    (if (is-in-grid? start-x start-y end-x end-y grid)
      (loop [open-list start-open-list closed-list []]
        (let [current (lowest-f-score open-list)
              new-open-list (vec (remove #(= % current) open-list))
              new-closed-list (conj closed-list current)]
          (if (and (= (:x current) end-x) (= (:y current) end-y))
             (conj closed-list current)
             (recur (vec (clean-dupes (concat new-open-list (keep #(calc-neighbours % current new-closed-list end-x end-y) (neighbours (:x current) (:y current) grid)))))
                  new-closed-list))))
      [{:x 0 :y 0 :p -1}])))

(defn retrace-path [a*-paths]
  (loop [acc [] current-pos (- (count a*-paths) 1)]
    (if (not= current-pos nil)
      (recur (conj acc (dissoc (get a*-paths current-pos) :g :f :p)) (:p (get a*-paths current-pos)))
      (reverse (drop-last acc)))))

(defn add-path-to-grid [path grid]
  (let [path-size (count path)]
    (loop [grid-with-path grid counter 0]
      (cond
        (= counter path-size) grid-with-path
        (= counter (dec path-size)) (recur (assoc-in grid-with-path [(:x (nth path counter)) (:y (nth path counter)) :path] "end") (inc counter))
        (= counter 0) (recur (assoc-in grid-with-path [(:x (nth path counter)) (:y (nth path counter)) :path] "start") (inc counter))
        :else (recur (assoc-in grid-with-path [(:x (nth path counter)) (:y (nth path counter)) :path] true) (inc counter))))))

(defn solve-grid [start-x start-y end-x end-y grid]
  (let [a*-paths (a* start-x start-y end-x end-y grid)]
    (add-path-to-grid (retrace-path a*-paths) grid)))
