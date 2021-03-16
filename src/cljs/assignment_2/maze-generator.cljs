(ns assignment-2.maze-generator "Maze Generator"
  (:require [clojure.string :as str]))

; Maze Grid Creation
; -------------------------------------------------------------------------------------------------------------------
(defn make-a-row [columns]
  (loop [count 0 row []]
      (if (= columns count)
          row
          (recur (inc count) (conj row {:north 0 :east 0 :south 0 :west 0 :visited 0 :distance -1})))))

(defn make-a-grid [rows columns]
  (loop [count 0 grid []]
      (if (= rows count)
          grid
          (recur (inc count) (conj grid (make-a-row columns))))))

(def grid (atom (make-a-grid 20 20)))

(defn reset-grid []
    (reset! grid (make-a-grid 20 20)))

; Maze Helper Functions
; -------------------------------------------------------------------------------------------------------------------
(defn lookup-x-carve [direction]
  (let [dir-x-lookup {"N" 1 "E" 0 "S" -1 "W" 0}]
    (get dir-x-lookup direction)))

(defn lookup-y-carve [direction]
  (let [dir-y-lookup {"N" 0 "E" 1 "S" 0 "W" -1}]
    (get dir-y-lookup direction)))

(defn toprow? [row no-of-rows]
  (= row (- no-of-rows 1)))

(defn lastcolumn? [col no-of-cols]
  (= col (- no-of-cols 1)))

; Maze Generation Algorithms
; -------------------------------------------------------------------------------------------------------------------
(defn binary-tree
  ([row] (binary-tree row 0))
  ([row col]
   ; store number of rows and number of columns
   (let [no-of-rows (count @grid) no-of-cols (count (first @grid))]
       (cond
           ; above top row return maze
           (= row no-of-rows) @grid
           ; top row && last column do nothing
           (and (toprow? row no-of-rows) (lastcolumn? col no-of-cols)) 0
           ; top row carve east
           (toprow? row no-of-rows) (do
                                        (swap! grid assoc-in [row col :east] 1)
                                        (swap! grid assoc-in [row (+ col 1) :west] 1))
           ; not top row && last column carve north
           (lastcolumn? col no-of-cols) (do
                                            (swap! grid assoc-in [row col :north] 1)
                                            (swap! grid assoc-in [(+ row 1) col :south] 1))
           ; not top row carve north or east
           :else
           (if (= 0 (rand-int 2))
               (do
                   (swap! grid assoc-in [row col :east] 1)
                   (swap! grid assoc-in [row (+ col 1) :west] 1))
               (do
                   (swap! grid assoc-in [row col :north] 1)
                   (swap! grid assoc-in [(+ row 1) col :south] 1)))))))

(defn recursive-backtracker [x y]
  (let [directions (shuffle ["N" "E" "S" "W"])
        no-of-rows (count @grid)
        no-of-cols (count (first @grid))]
    (dotimes [direction (count directions)]
      (do
        (swap! grid assoc-in [x y :visited] 1)
        (let [nx (+ x (lookup-x-carve (get directions direction)))
              ny (+ y (lookup-y-carve (get directions direction)))]
          (if (and (<= 0 nx (- no-of-rows 1)) (<= 0 ny (- no-of-cols 1)) (= (:visited (get-in @grid [nx ny])) 0))
             (cond
                  (= (get directions direction) "N") (do
                                                       (swap! grid assoc-in [x y :north] 1)
                                                       (swap! grid assoc-in [nx ny :south] 1)
                                                       ; (println (print-as-text @grid))
                                                       ; (print x)(print " ")(println y)
                                                       ; (print nx)(print " ")(println ny)
                                                       ; (println (:visited (get-in @grid [nx ny])))
                                                       (recursive-backtracker nx ny))
                  (= (get directions direction) "E") (do
                                                       (swap! grid assoc-in [x y :east] 1)
                                                       (swap! grid assoc-in [nx ny :west] 1)
                                                       (swap! grid assoc-in [x y :visited] 1)
                                                       ; (println (print-as-text @grid))
                                                       ; (print x)(print " ")(println y)
                                                       ; (print nx)(print " ")(println ny)
                                                       ; (println (:visited (get-in @grid [nx ny])))
                                                       (recursive-backtracker nx ny))
                  (= (get directions direction) "S") (do
                                                       (swap! grid assoc-in [x y :south] 1)
                                                       (swap! grid assoc-in [nx ny :north] 1)
                                                       ; (println (print-as-text @grid))
                                                       ; (print x)(print " ")(println y)
                                                       ; (print nx)(print " ")(println ny)
                                                       ; (println (:visited (get-in @grid [nx ny])))
                                                       (recursive-backtracker nx ny))
                  (= (get directions direction) "W") (do
                                                       (swap! grid assoc-in [x y :west] 1)
                                                       (swap! grid assoc-in [nx ny :east] 1)
                                                       ; (println (print-as-text @grid))
                                                       ; (print x)(print " ")(println y)
                                                       ; (print nx)(print " ")(println ny)
                                                       ; (println (:visited (get-in @grid [nx ny])))
                                                       (recursive-backtracker nx ny)))))))))


; Maze Generation Entry Function
; -------------------------------------------------------------------------------------------------------------------
(defn carve-passages
  ([] (carve-passages 0 "bt"))
  ([row algorithm]
   {:post [(reset-grid)]}
   (if (= algorithm "bt")
     (if (= row (count @grid))
         @grid
         (do
            (dotimes [col (count (first @grid))]
                (binary-tree row col))
            (carve-passages (inc row) algorithm)))))
  ([x y algorithm]
   {:post [(reset-grid)]}
   (if (= algorithm "rb")
     (if (= x -1)
       @grid
       (do
         (recursive-backtracker x y)
         (carve-passages -1 0 "rb"))))))

; (def maze-string (print-as-text (carve-passages)))
;
; (def maze (map (partial apply str) (partition 17 maze-string)))
;
; (map #(.fillText ctx % 100 %2) maze (iterate #(+ % 10) 10))
