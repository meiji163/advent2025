(ns advent.day1
  (:require [clojure.string :as str]))

(defn parse [line]
  (let [dir (first line)
        num (subs line 1)
        sgn (if (= \L dir) -1 1)]
    (* sgn (parse-long num))))

(defn solve1 [nums]
  (count (filter (fn [n] (= 0 (mod (+ n 50) 100)))
           (reductions + nums))))

(defn red2 [acc n]
  (let [[count pos] acc
        next-pos (mod (+ pos n) 100)
        q (quot (if (pos? n)
                  (+ pos n)
                  (- (mod (- 100 pos) 100) n))
                100)]
    [(+ count q) next-pos]))

(defn solve2 [nums]
  (first (reduce red2 [0 50] nums)))

(defn -main []
  (let [input (slurp "input/day1.txt")
        nums (map parse (str/split-lines input))]
    (solve2 nums)))

(-main)
;; => 6616

(comment
  (def test-input
    "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

  )
