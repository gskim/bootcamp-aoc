(ns aoc2020-1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn refine-input [input]
  (s/split-lines input))

(defn get-input []
  (-> "2020_day1.txt"
      io/resource
      slurp
      s/split-lines))

(defn make-two-pair [input]
  (let [pairs   (for [x input
                      y input]
                  [x y])]
    (filter (fn [[x y]] (not= x y)) pairs)))

(defn make-three-pair [input]
  (let [pairs   (for [x input
                      y input
                      z input]
                  [x y z])]
    (filter (fn [[x y z]] (and (not= x y) (not= x z) (not= y z) (not= x y z))) pairs)))

(defn init-state [data]
  {:data          data
   :current-index 0})

(defn increse-current-index [state]
  (update state :current-index inc))

(defn is-not-sum-2020? [{:keys [data current-index]}]
  (let [current-data (nth data current-index)]
    (not= 2020 (apply + current-data))))

(defn get-sum-2020 [state]
  (->> state
       (iterate increse-current-index)
       (drop-while is-not-sum-2020?)
       first))

(defn multiplication [{:keys [data current-index]}]
  (let [target (nth data current-index)]
    (apply * target)))

(defn part1 []
  (->> (get-input)
       (map #(Integer/parseInt %))
       make-two-pair
       init-state
       get-sum-2020
       multiplication))

(defn part2 []
  (->> (get-input)
       (map #(Integer/parseInt %))
       make-three-pair
       init-state
       get-sum-2020
       multiplication))

(comment
  (part1)
  (part2))