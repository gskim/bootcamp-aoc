(ns aoc2020_4
  (:require [clojure.string :as s] [clojure.java.io :as io] [clojure.spec.alpha :as spec]))



(spec/def :passport/byr (fn [v] (and (>= (Integer. v) 1920) (<= (Integer. v) 2002))))
(spec/def :passport/iyr (fn [v] (and (>= (Integer. v) 2010) (<= (Integer. v) 2020))))
(spec/def :passport/eyr (fn [v] (and (>= (Integer. v) 2020) (<= (Integer. v) 2030))))
(spec/def :passport/hgt (fn [v] (or (and (s/ends-with? v "cm") (>= (Integer. (s/replace v #"cm" "")) 150) (<= (Integer. (s/replace v #"cm" "")) 193))
                                    (and (s/ends-with? v "in") (>= (Integer. (s/replace v #"in" "")) 59) (<= (Integer. (s/replace v #"in" "")) 76)))))
(spec/def :passport/hcl (fn [v] (re-matches #"#[0-9|a-f]{6}" v)))
(spec/def :passport/ecl (fn [v] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v)))
(spec/def :passport/pid (fn [v] (re-matches #"[0-9]{9}" v)))
(spec/def :passport/cid string?)

(def passport-require-keys #{:passport/byr
                             :passport/iyr
                             :passport/eyr
                             :passport/hgt
                             :passport/hcl
                             :passport/ecl
                             :passport/pid})

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

(defn keyword-map-input-data [input]
  (->> input
       (map #(s/replace % #"\n" " "))
       (map #(s/split % #" "))
       (map (fn [data] (reduce (fn [acc kv] (let [[k v] (s/split kv #":")] (assoc acc (keyword "passport" k) v))) {} data)))))

(comment
  (re-matches #"#[0-9|a-f]{6}" "#123123")
  (re-matches #"#[0-9|a-f]{6}" "#1231234")
  (if (and (s/ends-with? "172cm" "cm") (>= (Integer. (s/replace "172cm" #"cm" "")) 150) (<= (Integer. (s/replace "172cm" #"cm" "")) 193)) true false)
  (contains? #{:amb "blu" "brn" "gry" "grn" "hzl" "oth"} #{"blu"})
  (= (count (last (re-find #"#[0-9|a-f]{6}" "#12345612"))) 6)
  (re-find #"#[0-9|a-f]{6}" "#012323")
  (int? (re-find #"\d" "a123"))
  (int? 123)
  (and (>= (Integer. "2000") 1910) (<= (Integer. "2000") 2002))
  (last (re-find #"#([0-9a-z][0-9a-z][0-9a-z][0-9a-z][0-9a-z][0-9a-z])" "#123qwe"))
  (let [v #:passport{:hgt  "178cm"
                     :hcl  "#623a2f"
                     :pid  "562756202"
                     :eyr  "2020"
                     :byr  "1929"
                     :ecl  "oth"
                     :ecla "oth"
                     :iyr  "2013"}]
    (every? (set (keys v)) passport-require-keys)))

(comment
  "part2 use spec"
  (->> (get-input)
       keyword-map-input-data
       (filter #(spec/valid? :passport/available %))
       count))

(comment
  "part1"
  (->> (get-input)
       keyword-map-input-data
       (filter #(every? (set (keys %)) passport-require-keys))
       count))

