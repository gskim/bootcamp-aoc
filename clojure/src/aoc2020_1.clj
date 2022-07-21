(ns aoc2020-1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn refine-input [input]
  (s/split-lines input))

(defn get-input []
  (->> "2020_day1.sample.txt"
       io/resource
       slurp
       s/split-lines
       (map #(Integer/parseInt %))))

(defn make-two-pair [input]
  (for [x     input
        y     input
        :when (= (+ x y) 2020)]
    [x y]))

(defn make-three-pair [input]
  (for [x     input
        y     input
        z     input
        :when (= (+ x y z) 2020)]
    [x y z]))

(defn multiplication [data]
  (apply * data))

(defn part1 []
  (->> (get-input)
       make-two-pair
       first
       multiplication))

(defn part2 []
  (->> (get-input)
       make-three-pair
       first
       multiplication))

(comment
  (part1)
  (part2))