(ns assignment-1.mazes "Mazes"
  (:require [clojure.string :as str]))

(defn make-a-row [columns]
  (loop [count 0 row []]
      (if (= columns count)
          row
          (recur (inc count) (conj row {:north 0 :east 0 :south 0 :west 0 :visited 0})))))

(defn make-a-grid [rows columns]
  (loop [count 0 grid []]
      (if (= rows count)
          grid
          (recur (inc count) (conj grid (make-a-row columns))))))

(def grid (atom (make-a-grid 10 10)))

(defn make-a-cell
  ([row] (make-a-cell row 0))
  ([row col]
   ; store size of grid and number of cells
   (let [size (count @grid) cells (count (first @grid))]
       ; above top row return maze
       (cond
           (= row size) @grid
           ; top row && last cell do nothing
           (and (= row (- size 1)) (= col (- cells 1))) 0
           ; top row carve east
           (=  row (- size 1)) (do
                                   (swap! grid assoc-in [row col :east] 1)
                                   (swap! grid assoc-in [row (+ col 1) :west] 1))
           ; not top row && last cell carve north
           (= col (- cells 1)) (do
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

(defn carve-passages
  ([] (carve-passages 0))
  ([row]
   (if (= row (count @grid))
       @grid
       (do
           (dotimes [col (count (first @grid))]
               (make-a-cell row col))
           (carve-passages (inc row))))))

(defn print-cell-body [cell]
    (if (= 1 (:east cell))
        "    "
        "   |"))

(defn print-cell-bottom [cell]
    (if (= 1 (:south cell))
        "   +"
        "---+"))

(defn top-row
    ([c] (top-row c "+"))
    ([c row]

     (if (= 0 c)
         (str row "\n")
         (top-row (dec c) (str row "---+")))))

(defn print-as-text
    [maze]

    (loop [row (- (count maze) 1)
           final (top-row (count (maze 0)))]
        (if (< row 0)
            final
            (recur
                (dec row)
                (str final
                     "|"
                     (str/join (map #(print-cell-body %) (nth maze row)))
                     "\n+"
                     (str/join (map #(print-cell-bottom %) (nth maze row)))
                     "\n")))))


(def maze-string (print-as-text (carve-passages)))

(def maze (map (partial apply str) (partition 17 maze-string)))

(map #(.fillText ctx % 100 %2) maze (iterate #(+ % 10) 10))
