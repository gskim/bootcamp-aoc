(ns aoc2018-1
  (:require [clojure.string :as s]))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(def sample1 "resources/day1.sample.txt")
(def sample2 "resources/day1.txt")

(defn sum "두 입력값의 합을 return" [a b]
  (+ a b))

(defn get-data []
  (let [txt (slurp sample2)]
    (->> (s/split txt #"\r\n")
         (map read-string))))

(defn part1 []
  (->> (get-data)
       (reduce +)))

(println (part1))

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(defn duplicate-value
  "중복되는 값을 return" [list]
  (reduce (fn [acc v]
            (if (get acc v)
              (reduced v)
              (conj acc v)))
          #{} list))

(defn part2 []
  (->> (get-data)
       (cycle)
       (reductions sum)
       duplicate-value))

(println (part2))
