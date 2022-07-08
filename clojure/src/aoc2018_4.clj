(ns aoc2018_4
  (:require (clojure [string :as s])))

(def sample "resources/day4.txt")

(def input (->> (slurp sample)
                s/split-lines))

(defn is-guard-info [description]
  (s/starts-with? description "G"))

(defn get-guard-id [description]
  (->> (re-matches #"Guard #(\d+) begins shift" description)
       last
       Integer.))

(defn mapping-data [[date hour min description]]
  (let [is-guard? (is-guard-info description)]
    {:id       (Long. (str (s/replace date #"-" "") hour min))
     :min      min
     :type     (if is-guard? "guard" "time")
     :guard-id (if is-guard? (get-guard-id description) nil)}))

(defn redefine-data [string]
  (-> (re-matches #"\[(\d+-\d+-\d+) (\d+)\:(\d+)] (.*)" string)
      next
      mapping-data))

(defn order-by-id-asc [mapping-data-list]
  (sort-by :id mapping-data-list))

(defn all-min-by-min-range [[start-min end-min]]
  (range (Integer. (:min start-min)) (Integer. (:min end-min))))

(defn get-all-sleep-mins [times]
  (->> times
       (partition-all 2)
       (map all-min-by-min-range)))

(defn make-min-group-by-guard-id [partition-list]
  (map (fn [[guard times]] {:guard-id (:guard-id (first guard))
                            :times    (get-all-sleep-mins times)}) partition-list))

(defn time-list-group-by-guard-id [mapping-data-list]
  (->> (partition-by #(= "guard" (:type %)) mapping-data-list)
       (partition 2)
       (make-min-group-by-guard-id)))

(defn merge-data [min-guard-data-list]
  (->> min-guard-data-list
       (group-by :guard-id)
       vals
       (map #(->> {:guard-id (:guard-id (first %))
                   :times    (mapcat :times %)}))))

(defn assoc-total-sleep-minute [data]
  (assoc data :sleep-cnt (reduce (fn [acc minutes] (+ acc (count minutes))) 0 (:times data))))

(defn guard-who-sleeps-the-most [data-list]
  (first (sort-by :sleep-cnt > data-list)))

(defn guard-who-fequently-sleep-the-most [data-list]
  (last (sort-by :cnt data-list)))

(defn parse-minute-count [[minute cnt]]
  {:minute minute
   :cnt    cnt})

(defn most-frequency-minute-count [data]
  (->> (flatten (:times data))
       frequencies
       (sort-by val)
       last
       (parse-minute-count)))

(defn assoc-most-frequency-minute-and-count [data]
  (merge data (most-frequency-minute-count data)))

(defn reprocessing-input [input]
  (->> input
       (map redefine-data)
       (order-by-id-asc)
       (time-list-group-by-guard-id)
       (merge-data)))

(defn part1 []
  (->> input
       (reprocessing-input)
       (map assoc-total-sleep-minute)
       (guard-who-sleeps-the-most)
       (#(* (:guard-id %) (:minute (most-frequency-minute-count %))))))

(defn part2 []
  (->> input
       (reprocessing-input)
       (map assoc-most-frequency-minute-and-count)
       (guard-who-fequently-sleep-the-most)
       (#(* (:guard-id %) (:minute %)))))



(comment
  (part1)
  (part2))
