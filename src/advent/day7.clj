(ns advent.day7
  (:require [clojure.string :as str])
  (:require [advent.utils :as util]))

(defn parse [s]
  (let [lines (vec (map vec (str/split-lines s)))
        start (first
                 (util/indexes= \S (first lines)))]
    [lines start]))

(defn step [beams row]
  (let [splitters (util/indexes= \^ row)
        f (fn [i]
            (if (.contains splitters i)
              [(dec i) (inc i)]
              [i]))
        v (map f beams)
        splits (count (filter #(= 2 (count %)) v))
        new-beams (->> v
                       (flatten)
                       (dedupe)
                       (vec))]
    [new-beams splits]))

(defn step2! [paths row]
  (let [splitters (util/indexes= \^ row)
        len (count paths)]
    (doseq [i (range 0 len)]
      (when (.contains splitters i)
        (do (aset paths (dec i)
                  (+ (aget paths (dec i)) (aget paths i)))
            (aset paths (inc i)
                  (+ (aget paths (inc i)) (aget paths i))))))
    (doseq [i splitters]
      (aset paths i 0))
    ))

(defn solve1 [rows start]
  (let [f (fn [[beams n] row]
            (let [[new-beams splits] (step beams row)]
              [new-beams (+ n splits)]))]
    (second
     (reduce f [[start] 0] rows))))

(defn solve2 [rows start]
  (let [len (count (first rows))
        paths (make-array Long/TYPE len)
        _ (aset paths start 1)]
    (doseq [row rows]
      (println (vec paths))
      (step2! paths row))
    (reduce + paths)))

(defn -main []
  (let [[rows start] (parse (slurp "input/day7.txt"))]
    (solve1 rows start)
    (solve2 rows start))
  ;; => 15650261281478
  )

(comment
  (def test-input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

  (let [[rows start] (parse test-input)]
    (solve1 rows start)
    (solve2 rows start)
    )

  (step [5 6 8] (vec "......^.^......")))
