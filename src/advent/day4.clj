(ns advent.day4
  (:require [clojure.string :as str]))

(defn parse-grid [ll]
  (->> ll
       str/split-lines
       (map vec)
       vec))

(defn in-grid? [dims xy]
  (let [[xdim ydim] dims
        [x y] xy]
    (and (<= 0 x) (<= 0 y)
         (> xdim x) (> ydim y))))

(defn grid-dims [grid]
  [(count grid) (count (first grid))])

(defn neighbors [grid xy]
  (let [dims (grid-dims grid)
        [x y] xy]
    (filter (partial in-grid? dims)
            [[(inc x) y] [x (inc y)]
             [(dec x) y] [x (dec y)]
             [(inc x) (inc y)] [(inc x) (dec y)]
             [(dec x) (inc y)] [(dec x) (dec y)]]
            )))

(defn gget [grid xy]
  ((grid (first xy)) (second xy)))

(defn valid? [grid xy]
  (let [[x y] xy]
    (and (= \@ (gget grid xy))
         (> 4 (count
               (filter #(= \@ (gget grid %))
                       (neighbors grid xy)))))
    ))

(defn solve1 [grid]
  (let [[h w] (grid-dims grid)]
    (count
     (filter (partial valid? grid)
             (for [i (range 0 h)
                   j (range 0 w)]
               [i j])))
    ))

(defn grid-update [grid xys]
  (let [xmap (->> xys 
                  (group-by first)
                  (map (fn [[k v]] [k (map second v)])))]
    (reduce (fn [grid-acc [x ys]]
              (let [row (grid-acc x)
                    updates (interleave ys (repeat \.))
                    new-row (apply (partial assoc row) updates)]
                (assoc grid-acc x new-row)))
            grid
            xmap)))

(defn helper [grid coords n]
  (let [[h w] (grid-dims grid)
        
        to-rem (vec (filter (partial valid? grid) coords))

        to-rem-nbrs (->> to-rem
                         (map #(neighbors grid %))
                         (apply concat)
                         distinct
                         (filter #(= \@ (gget grid %))))

        next-grid (grid-update grid to-rem)]
    (if (empty? to-rem-nbrs)
      n 

      (recur next-grid
             to-rem-nbrs
             (+ n (count to-rem))))))

(defn solve2 [grid]
  (let [[h w] (grid-dims grid)
        coords (for [i (range 0 h)
                     j (range 0 w)
                     :when (valid? grid [i j])]
                 [i j])]
    (helper grid coords 0)))

(comment
  (def test-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")
  
  (def test-grid (parse-grid test-input))
  (solve1 test-grid)
  ;; => 13

  (solve2 test-grid)
  ;; => 43

  (grid-update test-grid [[0 2] [0 7]]))


(defn -main []
  (let [grid (parse-grid (slurp "input/day4.txt"))]
    (solve1 grid)
    ;; => 1397
    (solve2 grid)
    ;; => 8758
    ))
