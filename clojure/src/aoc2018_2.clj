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

(defn refine-input [input]
  (s/split-lines input))


(defn twice
  "전달받은 문자열에 중복문자 2개가 있는지 확인"
  [s]
  (loop [ss (s/split s #"") st #{}]
    (if (empty? ss)
      nil
      (if (get st (first ss))
        true
        (recur (rest ss) (conj st (first ss)))))))

(defn three-times
  "전달받은 문자열에 중복문자 3개가 있는지 확인하고 해당 문자 return"
  [s]
  (loop [ss (s/split s #"") mp {} v []]
    (if (empty? ss)
      v
      (if (= 2 (Integer. (get mp (first ss) 0)))
        (recur (rest ss) (merge mp {(first ss) (inc (get mp (first ss) 0))}) (conj v (first ss)))
        (recur (rest ss) (merge mp {(first ss) (inc (get mp (first ss) 0))}) v)))))

(defn remove-char
  "전달받은 문자열에 target문자를 삭제"
  [remove-targets st]
  (loop [remove-targets remove-targets st st]
    (if (empty? remove-targets)
      st
      (recur (rest remove-targets) (s/replace st (first remove-targets) "")))))

(defn count-change
  "전달받은 문자열에 중복문자 여부에 따라 mp {:two :three} 의 값을 inc"
  [mp str]
  (let [two (mp :two) three (mp :three)]
    (let [three-words (three-times str)]
      (let [twice? (twice (remove-char three-words str))]
        {:two (if (= twice? true) (inc two) two) :three (if (> (count three-words) 0) (inc three) three)}))))


(defn part1 []
  (->> (slurp sample)
       (refine-input)
       (reduce count-change {:two 0 :three 0})
       (#(* (% :two) (% :three)))))

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


(defn split-string-and-mapping [string]
  (s/split string #""))

(defn just-one-diff?
  "비교문자열 2개중 하나만 다를경우 해당 index를 return.\n
   없거나 2개이상일 경우 return nil
   "
  [target-a target-b]
  (if (not (= (count target-a) (count target-b)))
    nil
    (let [not-equal-list (filter (fn [i] (not (= (target-a i) (target-b i)))) (range (count target-a)))]
      (if (= 1 (count not-equal-list))
        (first not-equal-list)
        nil))))

(defn vec-remove
  "전달받은 vector에 pos 위치값 제거해서 return"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn get-diff-vector
  "전달받은 target이 list에 하나만 다른값이 있는경우 다른 문자가 제거된값을 return"
  [target list]
  (loop [r list]
    (if (empty? r)
      nil
      (let [diff? (just-one-diff? target (first r))]
        (if (nil? diff?)
          (recur (rest r))
          (vec-remove diff? target))))))

(defn compare-loop [list]
  (loop [target (first list) others (rest list)]
    (if (empty? others)
      nil
      (let [result (get-diff-vector target others)]
        (if (nil? result)
          (recur (first others) (rest others))
          result)))))

(defn find-diff-and-make-result [input]
  (if (< (count input) 2)
    nil
    (let [result (compare-loop input)]
      (s/join "" result))))

(defn part2 []
  (->> (slurp sample)
       (refine-input)
       (map split-string-and-mapping)
       (find-diff-and-make-result)))

(comment
  (just-one-diff? ["a" "a" "b" "b" "c"] ["a" "a" "b" "b" "e"])
  (find-diff-and-make-result (map split-string-and-mapping '("abcde" "fghij" "fguij")))
  (part2))