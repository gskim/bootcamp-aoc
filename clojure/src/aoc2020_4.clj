(ns aoc2020_4
  (:require [clojure.string :as s] [clojure.java.io :as io] [clojure.spec.alpha :as spec]))


(spec/def ::inch #(spec/int-in-range? 59 (inc 76) (:height %)))
(spec/def ::centimeter #(spec/int-in-range? 150 (inc 193) (:height %)))
(spec/def :passport/byr #(spec/int-in-range? 1920 (inc 2002) %))
(spec/def :passport/iyr #(spec/int-in-range? 2010 (inc 2020) %))
(spec/def :passport/eyr #(spec/int-in-range? 2020 (inc 2030) %))
(spec/def :passport/hgt #(case (:type %)
                           "cm" ::centimeter
                           "in" ::inch
                           false))
(spec/def :passport/hcl #(re-matches #"#[0-9|a-f]{6}" %))
(spec/def :passport/ecl #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %))
(spec/def :passport/pid #(re-matches #"[0-9]{9}" %))
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


(defn height-string->map [s]
  (let [[_ height type] (re-matches #"([0-9]+)(in|cm)" s)]
    (if (nil? type)
      nil
      {:type   type
       :height (Integer. height)})))

(defn replace-value-by-key [k v]
  (case k
    ("iyr" "eyr" "byr") (Integer. v)
    "hgt" (height-string->map v)
    v))

(defn get-input [] (-> "2020_day4.txt" io/resource slurp (s/split #"\n\n")))

(defn redefined-keword-value [string-v]
  (->> (map #(let [[k v] (s/split % #":")] [(keyword "passport" k) (replace-value-by-key k v)]) string-v)
       (into {})))

(defn keyword-map-input-data [input]
  (->> input
       (map #(s/replace % #"\n" " "))
       (map #(s/split % #" "))
       (map redefined-keword-value)))

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

