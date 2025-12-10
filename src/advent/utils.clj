(ns advent.utils
  (:require [clojure.string :as str]))

(defn parse-range [s]
  "parse range of the form start-end"
  (map parse-long (str/split (str/trim s) #"-")))

(defn to-digits [n]
  (->> n
       str
       (map (fn [c] (Character/digit c 10)))))

(defn from-digits [ds]
  (reduce (fn [acc d] (+' (*' 10 acc) d)) ds))

(defn indexes= [x coll]
  "The indexes of coll where the value equals x"
  (keep-indexed #(when (= x %2) %1) coll))
