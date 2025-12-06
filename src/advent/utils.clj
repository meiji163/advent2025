(ns advent.utils
  (:require [clojure.string :as str]))

(defn parse-range [s]
  (map parse-long (str/split (str/trim s) #"-")))

(defn to-digits [n]
  (->> n
       str
       (map (fn [c] (Character/digit c 10)))))

(defn from-digits [ds]
  (reduce (fn [acc d] (+' (*' 10 acc) d)) ds))
