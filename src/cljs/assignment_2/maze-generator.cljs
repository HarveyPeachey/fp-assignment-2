(ns assignment-2.maze-generator "Maze Generator"
  (:require [clojure.string :as str]))

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

; (def maze-string (print-as-text (carve-passages)))
;
; (def maze (map (partial apply str) (partition 17 maze-string)))
;
; (map #(.fillText ctx % 100 %2) maze (iterate #(+ % 10) 10))
