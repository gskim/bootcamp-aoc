(ns aoc2020_4
  (:require [clojure.string :as s] [clojure.java.io :as io] [clojure.spec.alpha :as spec]))

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


(spec/def :passport/byr (fn [v] (and (= (count v) 4) (>= (Integer. v) 1920) (<= (Integer. v) 2002))))
(spec/def :passport/iyr (fn [v] (and (= (count v) 4) (>= (Integer. v) 2010) (<= (Integer. v) 2020))))
(spec/def :passport/eyr (fn [v] (and (= (count v) 4) (>= (Integer. v) 2020) (<= (Integer. v) 2030))))
(spec/def :passport/hgt (fn [v] (or (and (s/ends-with? v "cm") (>= (Integer. (s/replace v #"cm" "")) 150) (<= (Integer. (s/replace v #"cm" "")) 193))
                                    (and (s/ends-with? v "in") (>= (Integer. (s/replace v #"in" "")) 59) (<= (Integer. (s/replace v #"in" "")) 76)))))
(spec/def :passport/hcl (fn [v] (re-find #"#[0-9|a-f]{6}" v)))
(spec/def :passport/ecl (fn [v] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v)))
(spec/def :passport/pid (fn [v] (and (s/starts-with? v "0") (re-find #"[0-9]{9}" v))))
(spec/def :passport/cid string?)

(spec/def :passport/available
  (spec/keys :req [:passport/byr
                   :passport/iyr
                   :passport/eyr
                   :passport/hgt
                   :passport/hcl
                   :passport/ecl
                   :passport/pid]
             :opt [:passport/cid]))

(defn get-input [] (-> "2020_day4.txt"
                       (io/resource)
                       (slurp)
                       (s/split #"\n\n")))

(defn parsed-input-data [input]
  (->> input
       (map #(s/replace % #"\n" " "))
       (map #(s/split % #" "))
       (map (fn [data] (reduce (fn [acc kv] (let [[k v] (s/split kv #":")] (assoc acc k v))) {} data)))))

(defn keyword-map-input-data [input]
  (->> input
       (map #(s/replace % #"\n" " "))
       (map #(s/split % #" "))
       (map (fn [data] (reduce (fn [acc kv] (let [[k v] (s/split kv #":")] (assoc acc (keyword "passport" k) v))) {} data)))))

(defn filter-by-has-passport-require-key [parsed-input-data]
  (let [passport-require-keys (filter (fn [v] (:require (val v))) passport-keys)]
    (filter (fn [v] (every? v (keys passport-require-keys))) parsed-input-data)))

(defn filter-by-passport-available [keyword-map-input-data]
  (filter (fn [v] (spec/valid? :passport/available v)) keyword-map-input-data))


(comment
  (if (and (s/ends-with? "172cm" "cm") (>= (Integer. (s/replace "172cm" #"cm" "")) 150) (<= (Integer. (s/replace "172cm" #"cm" "")) 193)) true false)
  (contains? #{:amb "blu" "brn" "gry" "grn" "hzl" "oth"} "blu")
  (= (count (last (re-find #"#[0-9|a-f]{6}" "#12345612"))) 6)
  (re-find #"#[0-9|a-f]{6}" "#012323")
  (int? (re-find #"\d" "a123"))
  (int? 123)
  (last (re-find #"#([0-9a-z][0-9a-z][0-9a-z][0-9a-z][0-9a-z][0-9a-z])" "#123qwe")))

(comment
  "part1 use spec"
  (->> (get-input)
       keyword-map-input-data
       #_filter-by-passport-available
       #_count))

(comment
  "part1"
  (->> (get-input)
       parsed-input-data
       filter-by-has-passport-require-key
       count))

