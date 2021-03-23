(ns assignment-2.maze-graphics "Maze Graphics"
  (:require [clojure.string :as str]))

(defn print-cell-body [cell]
    (if (nil? (:path cell))
      (if (= 1 (:east cell))
          "    "
          "   |")
      (if (= 1 (:east cell))
          " •  "
          " • |")))

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
