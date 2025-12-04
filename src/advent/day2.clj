(ns advent.day2
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math]))

(defn parse-range [s]
  (map parse-long (str/split (str/trim s) #"-")))

(defn parse [s]
  (map parse-range (str/split s #",")))

(defn to-digits [n]
  (->> n
       str
       (map (fn [c] (Character/digit c 10)))))

(defn from-digits [ds]
  (reduce (fn [acc d] (+ (* 10 acc) d)) ds))

(defn least-repeat2 [x]
  (let [digs (to-digits x)
        len (count digs)
        half (if (odd? len)
                    (/ (dec len) 2)
                    (/ len 2))]
    (from-digits
     (if (odd? len)
       (cons 1 (repeat half 0))
       (take half digs)))
    ))

(defn least-repeatn [range n]
  (let [[lo hi] range
        lo-digs (to-digits lo)
        hi-digs (to-digits hi)
        len (count digs)
        q (quot len n)
        r (rem len n)
        pattern (from-digits
                 (if (zero? r)
                   (take n digs)
                   (cons 1 (repeat (dec n) 0))))
        times (cond
                (= len 1) 2
                (zero? r) q
                :else (inc q))]
    [pattern times]))

(defn invalid-idsn [range n]
  (let [[lo hi] range
        [lr times] (least-repeatn lo n)
        seq (map #(repeatn % times) (iterate inc lr))
        ;; seq1 (drop-while #(< % lo) seq)
        ]
    (take-while #(<= % hi) seq)))

(defn invalid-ids-all [rng]
  (let [[lo hi] rng
        len (num-digits hi)
        half (inc (quot len 2))
        ids (for [n (range 1 half)]
              (invalid-idsn rng n))]
    (->> ids flatten distinct)))

(defn num-digits [n]
  (count (to-digits n)))

(defn double [n]
  (+ (* n (math/expt 10 (num-digits n)))
     n))

(defn repeatn [x n]
  (let [len (num-digits x)]
    (reduce (fn [acc y] (+ (* acc (math/expt 10 len)) y))
            (repeat n x))
    ))

(defn invalid-ids [range]
  (let [[lo hi] range
        lr (least-repeat2 lo)
        seq (drop-while
             #(<= (double %) lo)
             (iterate inc lr))]
    (take-while #(<= (double %) hi) seq)))

(defn solve1 [ranges]
  (reduce +
          (flatten (map
            (fn [range] (map double (invalid-ids range)))
            ranges))))

(defn solve2 [ranges]
  (flatten
   (map invalid-ids-all ranges)))

(comment
  (parse-range "11-22")
  (def test-input (parse "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124"))
  (solve1 test-input)
  ;; => 1227775543

  (solve2 test-input)
  (invalid-idsn [95 115] 1)

  (least-repeat2 16)
  (take 5 (iterate inc (least-repeat2 16)))
  (invalid-ids [16 35])
  (invalid-idsn [1 14] 1)
  )

(defn -main []
  (let [input (slurp "input/day2.txt")
        ranges (parse input)]

    (solve2 ranges))
  )

(-main)
;; => 25658628826


;; => 25663320820



;; => 8576933996


