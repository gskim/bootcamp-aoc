(ns aoc2020_8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))


(defn get-input [] (-> "2020_day8.sample.txt" io/resource slurp s/split-lines))

(defn parse-input [input]
  (->> input
       (map (fn [v] (s/split v #" ")))
       vec))

(defn mapping-by-id [parsed-input]
  (->> (map (fn [v]
              (let [input-v (get parsed-input v)]
                [v {:action (first input-v)
                    :cnt    (read-string (second input-v))}]))
            (range (count parsed-input)))
       (into {})))

(defn get-next-id [data now-id]
  (let [now (get data now-id)]
    (case (:action now)
      ("nop" "acc") (inc now-id)
      "jmp" (+ now-id (:cnt now)))))

(defn init-state []
  (let [data (->> (get-input)
                  parse-input
                  mapping-by-id)]
    {:data-map data
     :next-id  nil
     :sum      0
     :ids      []}))

(defn duplicate-id? [state]
  (let [next-id      (:next-id state)
        before-ids   (:ids state)
        distinct-ids (distinct (conj before-ids next-id))]
    (not= (count before-ids) (count distinct-ids))))

(defn update-next-id [state]
  (let [data-map (:data-map state)
        id       (peek (:ids state))
        next-id  (if (nil? id) 0 (get-next-id data-map id))]
    (update state :next-id (constantly next-id))))

(defn calc-action [state]
  (let [data-map (:data-map state)
        next-id  (:next-id state)
        data     (get data-map next-id)
        action   (:action data)
        cnt      (:cnt data)]
    (if (not (nil? next-id))
      (-> state
          (update :ids (fn [ids] (conj ids next-id)))
          (update :sum (fn [sum] (if (= action "acc") (+ sum cnt) sum))))
      state)))

(defn update-state [state]
  (-> state
      calc-action
      update-next-id))

(defn part1
  "part1"
  []
  (->> (init-state)
       (iterate update-state)
       (drop-while duplicate-id?)
       first
       :sum))

(comment
  (->> (get-input)
       parse-input
       mapping-by-id))

(comment
  "part1"
  (part1))
