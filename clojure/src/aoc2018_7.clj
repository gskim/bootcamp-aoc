(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))

(defn find-first-keys [input-data]
  (let [keys (map first input-data)
        vals (map last input-data)]
    (sort (st/difference (set keys) (set vals)))))

(defn group-by-last [parsed-input-data]
  (->> parsed-input-data
       (group-by last)
       (map (fn [v] {:key          (key v)
                     :waiting-keys (sort (map first (val v)))}))))

(defn input-data []
  (->> "day7.txt"
       (io/resource)
       (slurp)
       (clojure.string/split-lines)
       (map (fn [v] (let [[_ first second] (re-matches #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." v)]
                      [first second])))))

(defn get-ready-targets [done-target group-by-last]
  (let [update-group (map (fn [v]
                            (update v :waiting-keys #(remove (fn [x] (= x done-target)) %))) group-by-last)
        ready-keys   (->> update-group
                          (filter (fn [v] (empty? (:waiting-keys v))))
                          (map :key)
                          vec)]
    {:update-group (filter (fn [v] (not (some #{(:key v)} ready-keys))) update-group)
     :ready-keys   ready-keys}))

(defn make-order-vector [parsed-input-data]
  (let [[first-key & r] (find-first-keys parsed-input-data)]
    (loop [group-by-last (group-by-last parsed-input-data)
           target        first-key
           waiting       (vec r)
           v             [first-key]]
      (if (not (nil? target))
        (let [update-data (get-ready-targets target group-by-last)
              new-wating  (sort (distinct (concat (:ready-keys update-data) waiting)))]
          (recur (:update-group update-data) (first new-wating) (rest new-wating) (conj v (first new-wating))))
        v))))

(comment
  (->> (input-data)
       make-order-vector
       s/join))