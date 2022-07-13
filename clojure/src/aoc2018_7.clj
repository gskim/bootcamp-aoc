(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))

(defn find-first-targets [input-data]
  (let [first-targets (map first input-data)
        next-targets  (map last input-data)]
    (sort (st/difference (set first-targets) (set next-targets)))))

(defn group-by-last [parsed-input-data]
  (->> parsed-input-data
       (group-by last)
       (map (fn [v] {:target          (key v)
                     :waiting-targets (sort (map first (val v)))}))))

(defn input-data []
  (->> "day7.txt"
       (io/resource)
       (slurp)
       (clojure.string/split-lines)
       (map (fn [v] (let [[_ first second] (re-matches #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." v)]
                      [first second])))))

(defn get-ready-targets [done-target group-by-last]
  (let [update-group  (map (fn [v]
                             (update v :waiting-targets #(remove (fn [x] (= x done-target)) %))) group-by-last)
        ready-targets (->> update-group
                           (filter (fn [v] (empty? (:waiting-targets v))))
                           (map :target)
                           vec)]
    {:update-group  (filter (fn [v] (not (some #{(:target v)} ready-targets))) update-group)
     :ready-targets ready-targets}))

(defn calculator-order [parsed-input-data]
  (let [[first-target & r] (find-first-targets parsed-input-data)]
    (loop [group-by-last (group-by-last parsed-input-data)
           target        first-target
           waiting       (vec r)
           result        [first-target]]
      (if (not (nil? target))
        (let [update-data (get-ready-targets target group-by-last)
              new-wating  (sort (distinct (concat (:ready-targets update-data) waiting)))]
          (recur (:update-group update-data) (first new-wating) (rest new-wating) (conj result (first new-wating))))
        result))))

(comment
  (->> (input-data)
       calculator-order
       s/join))