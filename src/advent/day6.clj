(ns advent.day6
  (:require [clojure.string :as str])
  (:require [advent.utils :as util]))

(defn parse-nums [ll]
  (let [f (fn [l]
            (as-> l x
                  (str/split x #"\s+")
                  (map str/trim x)
                  (map parse-long x)
                  (filter #(not (nil? %)) x)
                  (vec x)))
        lines (map f ll)]
    (apply map vector lines)))

(comment
  (def test-input "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +" 
    )
  (let [[nums ops] (parse test-input)]
    (solve nums ops))
  ;; => 4277556

  (let [[nums2 ops2] (parse2 test-input)]
    (solve nums2 ops2))
  ;; => 3263827
  )

(defn parse-ops [l]
  (vec
   (map (fn [x]
          (if (= x "*")
            clojure.core/*
            clojure.core/+))
        (str/split l #"\s+"))))

(defn parse [s]
  (let [lines (str/split-lines s)
        l1 (take (dec (count lines)) lines)
        l2 (last lines)
        nums (parse-nums l1)
        ops (parse-ops l2)]
    [(vec nums) (vec ops)]))

(defn parse2 [s]
  (let [lines (str/split-lines s)
        l1 (take (dec (count lines)) lines)
        l2 (last lines)
        ops (parse-ops l2)
        max-idx (count (first l1))
        ;; the index of the operation char is the start of the num column
        idxs (as-> l2 x
               (char-array x)
               (keep-indexed #(when (not= %2 \space) %1) x)
               (vec x)
               (conj x (inc max-idx)))  ; add line end index
    
        nums (for [n (range 0 (dec (count idxs)))]
               (parse-num-col l1
                              (idxs n)              ;start index
                              (dec (idxs (inc n)))) ;end index
               )]
    [(vec nums) (vec ops)]))

(comment
 )

(defn parse-num-col [ll start end]
  (for [i (range start end)]
    (let [is-space? #(= " " %)
          col (map #(subs % i (inc i)) ll)
          col-trimmed (take-while #(not (is-space? %))
                       (drop-while is-space? col))
          digs (map #(if (= " " %)
                       0
                       (parse-long %))
                    col-trimmed)
          num (util/from-digits digs)]
      num)))

(defn solve [nums ops]
  (reduce +
          (for [i (range 0 (count nums))]
            (reduce (ops i) (nums i)))))

(defn -main []
  (let [input (slurp "input/day6.txt") 
        [nums1 ops1] (parse input)
        [nums2 ops2] (parse2 input)
        ]
    (solve nums1 ops1)
    (solve nums2 ops2))
  ;; => 4309240495780
  ;; => 9170286552289
  )
