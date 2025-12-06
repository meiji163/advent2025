(ns advent.day5
  (:require [clojure.string :as str])
  (:require [advent.utils :as util]))

(defn parse [s]
  (let [[srange sids] (str/split s #"\n\n")
        ids (->> sids
                 str/split-lines
                 (map parse-long)
                 vec)
        ranges (->> srange
                    str/split-lines
                    (map (comp vec util/parse-range))
                    sort
                    vec)]
    [ranges ids]))

;; index of the range which contains id,
;; or -1 if none is found
(defn fresh [ranges id]
  (reduce (fn [acc [lo hi]]
            (cond
              (and (>= id lo) (<= id hi))
              (reduced acc)

              (and (< id hi) (< id lo))
              (reduced -1)
              
              :else
              (inc acc)))
          0
          ranges))

(defn solve1 [ranges ids]
  (->> ids
       (map (partial fresh ranges))
       (filter #(>= % 0))
       (filter #(< % (count ranges)))
       count))

(defn merge-range [r1 r2]
  (let [[l1 h1] r1
        [l2 h2] r2]
    (if
        (< h1 l2) nil 
        ;; mergeable since h1 >= l2
        [l1 (max h1 h2)])))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn helper [ranges i]
  (if (>= (inc i) (count ranges))
    ranges
    ;; try to merge ranges[i] with ranges[i+1].
    ;; if they're not mergeable, then ranges[i] isn't mergeable with any
    ;; range in ranges[i+1...] because `ranges` is sorted.
    (let [r1 (ranges i)
          r2 (ranges (inc i))
          new-r1 (merge-range r1 r2)]
      (if (nil? new-r1)
        (recur ranges (inc i))
      
        ;; mergeable range found
        (let [new-ranges
              (vec-remove (+ i 1)
                          (assoc ranges i new-r1))]
          (recur new-ranges i))
        ))))

(defn solve2 [ranges]
  (reduce
   +
   (map (fn [[lo hi]] (inc (- hi lo)))
        (helper ranges 0))))

(comment
  (def test-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")
  (let [[ranges ids] (parse test-input)]
    (helper ranges 0))
  ;; => [[3 5] [10 20]]
  )

(defn -main []
  (let [[ranges ids] (parse (slurp "input/day5.txt"))]
    ;; (solve1 ranges ids)

    (solve2 ranges))
  ;; => 365804144481581
  )
