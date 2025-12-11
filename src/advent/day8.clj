(ns advent.day8
  (:require [clojure.string :as str])
  (:require [clojure.set :refer [union difference]])
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn sqr [x]
  (* x x))

(defn dist [p1 p2]
  (let [[x1 y1 z1] p1
        [x2 y2 z2] p2]
    (+ (sqr (- x2 x1))
       (sqr (- y2 y1))
       (sqr (- z2 z1)))
    ))

(defn distances [points]
  (let [len (count points)
        dists (for [i (range 0 len)
                    j (range i len)
                    :when (not= i j)]
                [[i j]
                 (dist (points i) (points j))])]
    (into (priority-map) dists)))

(defn parse [s]
  (let [lines (str/split-lines s)
        f (fn [l]
            (as-> l x
              (str/split x #",")
              (map str/trim x)
              (map parse-long x)
              (vec x)))]
    (vec (map f lines))))

(defn add-edge [adj e]
  (let [[u v] e
        adj-u (adj u)
        new-adj-u (cond (nil? adj-u) [v]
                        (.contains adj-u v) adj-u
                        :else
                        (conj adj-u v))]
    (assoc adj u new-adj-u)))

(defn build-graph [edges]
  (let [f (fn [adj [v u]]
            (-> adj
                (add-edge [u v])
                (add-edge [v u])))]
    (reduce f {} edges)))

(defn bfs-step [adj state]
  (let [nodes (state :frontier)
        vis (state :visited)
        nbr-list (for [n nodes] (adj n))
        nbrs (->> nbr-list
                  (flatten)
                  (filter #(not (nil? %)))
                  (filter #(not (contains? vis %)))
                  (into #{}))
        done (= 0 (count nbrs))]
    {:visited (union vis nodes)
     :frontier nbrs
     :done done}))

(defn bfs [adj state]
  (loop [s state]
    (if (= true (s :done))
      s
      
      (let [new-s (bfs-step adj s)]
        (recur new-s)))))

(defn components [adj nodes]
  (loop [vis #{}
         comps []]
    (let [start (first (difference nodes vis))]
      (if (nil? start)
        comps

        (let [state (bfs adj {:visited #{}
                              :frontier #{start}})
              new-vis (union vis (state :visited))
              new-comps (conj comps (state :visited))]
          (recur new-vis new-comps))))
    ))

(defn solve1 [points topn]
  (let [nodes (into #{} (range 0 (count points)))
        dists (distances points)
        edges (map first (take topn (seq dists)))
        adj (build-graph edges)
        comps (components adj nodes)]
    (->> comps
         (map count)
         (sort)
         (reverse)
         (take 3)
         (reduce *))))

(defn solve2 [points]
  (let [nodes (into #{} (range 0 (count points)))
        dists (distances points)

        ;; 6000 edges has 2 components, 7000 has 1 component.
        ;; We keep adding to the graph with 6000 edges until it has 1 component.
        edges (map first (take 7000 (seq dists)))
        [start-edges rest-edges] (mapv vec (split-at 6000 edges))

        adj (build-graph start-edges)
        comps (components adj nodes)

        component-index
        (fn [node]
          (first (filter #(contains? (comps %) node)
                         (range 0 (count comps)))))

        connects-graph?
        (fn [e]
          (let [[u v] e]
            (not= (component-index u)
                  (component-index v))))]

    (loop [i 0]
      (let [e (rest-edges i)]
        (if (connects-graph? e)
          (->> e
               (map #(nth points %))
               (map first)
               (reduce *))

          (recur (inc i)))))
    ))

(defn -main []
  (let [points (parse (slurp "input/day8.txt"))]
    ;; (solve1 points 1000)
    ;; => 80446
    (solve2 points)
    )
  )

(comment
  (def test-input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

  (let [points (parse test-input)
        nodes (into #{} (range 0 (count points)))
        dists (distances points)
        edges (map first (take 10 (seq dists)))
        adj (build-graph edges)
        state {:visited #{}
               :frontier #{1}}]
    (bfs adj state)
    (components adj nodes)
    )
  )
