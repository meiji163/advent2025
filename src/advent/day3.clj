(ns advent.day3
  (:require [clojure.string :as str]))

(def NUM-DIGITS 12)

(defn parse-line [l]
  (->> l
       (map str)
       (map #(parse-long %))
       vec))

(defn parse-lines [ll]
  (->> ll
       str/split-lines
       (map parse-line)))

(defn max-index [v]
  (second
   (reduce-kv (fn [[max-val max-idx] i x]
                (if (> x max-val)
                  [x i] 
                  [max-val max-idx]))
              [0 0]
              v)))

(defn from-digits [ds]
  (reduce (fn [acc d] (+' (*' 10 acc) d)) ds))

(defn helper [digs idxs pos]
  (if (>= pos NUM-DIGITS)  
    ;; done
    (from-digits
     (for [i idxs] (digs i)))

    ;; recursively add max digit
    (let [l (count digs)
          start-idx (if (empty? idxs)
                      0
                      (inc (last idxs)))
          end-idx (inc (- l (- NUM-DIGITS pos)))

          new-idx (+ start-idx
                     (max-index (subvec digs start-idx end-idx)))]
      (recur digs (conj idxs new-idx) (inc pos))
      )))

(defn candidates [nums]
  (let [l (count nums)]
    (for [i (range 0 l)
          j (range (inc i) l)]
      (+ (* 10 (nums i))
         (nums j))
      )))

(defn solve1 [nns]
  (->> nns
       (map #(apply max (candidates %)))
       (reduce +)))

(defn solve2 [nns]
  (->> nns
       (map #(helper % [] 0))
       (reduce +)))

(comment
  (def test-input (parse-lines "987654321111111
811111111111119
234234234234278
818181911112111"))
  (solve1 test-input)
  ;; => 357
  (solve2 test-input)
  ;; => 3121910778619
  )

(defn -main []
  (let [input (parse-lines (slurp "input/day3.txt"))]
    ;; (solve1 input)
    ;; => 16858
    (solve2 input)
    ;; => 167549941654721
    ))
