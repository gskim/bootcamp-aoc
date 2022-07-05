(ns aoc2018-2
  (:require [clojure.string :as s]))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(def sample "resources/day2.txt")

(defn make-data [data]
  (s/split-lines data))


(defn twice [s]
  (loop [ss (s/split s #"") st #{}]
    (if (empty? ss)
      nil
      (if (get st (first ss))
        true
        (recur (rest ss) (conj st (first ss)))))))

(defn three-times [s]
  (loop [ss (s/split s #"") mp {} v []]
    (if (empty? ss)
      v
      (if (= 2 (Integer. (get mp (first ss) 0)))
        (recur (rest ss) (merge mp {(first ss) (inc (get mp (first ss) 0))}) (conj v (first ss)))
        (recur (rest ss) (merge mp {(first ss) (inc (get mp (first ss) 0))}) v)))))

(defn remove-char [remove-targets st]
  (loop [remove-targets remove-targets st st]
    (if (empty? remove-targets)
      st
      (recur (rest remove-targets) (s/replace st (first remove-targets) "")))))

(defn count-change [mp str]
  (let [two (mp :two) three (mp :three)]
    (let [three-words (three-times str)]
      (let [twice? (twice (remove-char three-words str))]
        {:two (if (= twice? true) (inc two) two) :three (if (> (count three-words) 0) (inc three) three)}))))

(defn result [mp]
  (* (mp :two) (mp :three)))

(defn part1 []
  (->> (slurp sample)
       (make-data)
       (reduce count-change {:two 0 :three 0})
       (result)))

(comment
  (part1))

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################
