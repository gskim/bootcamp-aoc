(ns aoc2020_4
  (:require [clojure.string :as s] [clojure.java.io :as io]))

(def passport-keys {"byr" {:require   true
                           :condition {count 4
                                       min   1920
                                       max   2002}}
                    "iyr" {:require   true
                           :condition {count 4
                                       min   2010
                                       max   2020}}
                    "eyr" {:require   true
                           :condition {count 4
                                       min   2020
                                       max   2030}}
                    "hgt" {:require   true
                           :condition {"or-end-with" {"cm" {min 150
                                                            max 193}
                                                      "in" {min 59
                                                            max 76}}}}
                    "hcl" {:require   true
                           :condition {"regex" {#"#([0-9a-z])" {count 6}}}}
                    "ecl" {:require   true
                           :condition {"or-equal" ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]}}
                    "pid" {:require   true
                           :condition {"start-with" 0
                                       count      9}}
                    "cid" {:require   false
                           :condition {}}})

(defn get-input [] (-> "2020_day4.txt"
                       (io/resource)
                       (slurp)
                       (s/split #"\n\n")))

(defn parsed-input-data [input]
  (->> input
       (map #(s/replace % #"\n" " "))
       (map #(s/split % #" "))
       (map (fn [data] (reduce (fn [acc kv] (let [[k v] (s/split kv #":")] (assoc acc k v))) {} data)))))

(defn filter-by-has-passport-require-key [parsed-input-data]
  (let [passport-require-keys (filter (fn [v] (:require (val v))) passport-keys)]
    (filter (fn [v] (every? v (keys passport-require-keys))) parsed-input-data)))

(comment
  "part1"
  (->> (get-input)
       parsed-input-data
       filter-by-has-passport-require-key
       count))

