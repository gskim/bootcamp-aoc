(ns aoc2018_3
  (:require [clojure.string :as s]))

(def sample "resources/day3.sample.txt")

(comment
  (slurp sample)
  (count (s/split-lines (slurp sample))))


(defn get-increment-position-list [x y]
  (for [x (range x)
        y (range y)]
    [x y]))

(defn sum-position-list
  "position 목록에 전달받은 현재위치를 더해서 return\n
   input: [list ([0 0] [0 1] [0 2] [1 0] [1 1] [1 2]) position [2 1]]\n
   output: ([2 1] [2 2] [2 3] [3 1] [3 2] [3 3])
   "
  [list position]
  (map (fn [pos] [(+ (get pos 0) (get position 0)) (+ (get pos 1) (get position 1))]) list))


(comment
  (->> (s/split-lines (slurp sample))
       (#(let [list %]
           (map (fn [v] (s/split v #" ")) list)))
       (#(let [list %]
           (map (fn [v] {(keyword (s/replace (get v 2) ":" "")) {:start-pos (map read-string (s/split (s/replace (v 2) ":" "") #","))
                                                                 :id        (v 0)
                                                                 :grid      (map read-string (s/split (v 3) #"x"))}}) list)))
      ;;  (#(let [list %]
      ;;      (println list)
      ;;      (loop [list list
      ;;             mp   {}]
      ;;        (if (empty? list)
      ;;          mp
      ;;          (let [target (first list)]
      ;;            (recur (rest list)
      ;;                   (assoc mp (target :start-pos) (conj (get mp (target :start-pos) []) (target :id)))))))))
       ))

(for [x    (range 1 6)
      :let [y (* x x)
            z (* x x x)]]
  [x y z])

(comment
  (->>
   (let [x 2
         y 3]
     (for [x (range x)
           y (range y)]
       [x y])
    ;; 1,3 2,3
   ;;  1,4 2,4
   ;;  1,5 2,5 
     )
   (map (fn [v] [(+ (get v 0) 2) (+ (get v 1) 1)]))))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)




;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
