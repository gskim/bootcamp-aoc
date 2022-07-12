(ns aoc2018_6
  (:require [clojure.string :as s] [clojure.set :as st]  [clojure.java.io :as io]))

(defn get-input [] (-> "day6.txt"
                       (io/resource)
                       (slurp)
                       (s/split-lines)))

(defn get-angular-point
  "전달받은 좌표들의 가장 끝점을 return
   input ({:x 1, :y 1} {:x 1, :y 6} ... {:x 6, :y 10})
   output {:x1 1, :x2 8 :y1 1, :y2 10}
   "
  [coordinates]
  (let [x-vals (map :x coordinates)
        y-vals (map :y coordinates)]
    {:x1 (apply min x-vals)
     :x2 (apply max x-vals)
     :y1 (apply min y-vals)
     :y2 (apply max y-vals)}))

(defn end-include-range
  "end 가 포함된 range return
   input [1 5]
   output [1 2 3 4 5]
   "
  [start end]
  (range start (inc end)))

(defn get-all-coordinates
  "전달받은 좌표들의 모서리 좌표 안쪽 모든 좌표들을 return
   input ({:x 1, :y 1} {:x 1, :y 6} ... {:x 6, :y 10})
   output ({:x 1, :y 1} {:x 1, :y 2} {:x 1, :y 3} ... {:x 6, :y 10})
   "
  [coordinates]
  (let [angular-point (get-angular-point coordinates)
        [x1 x2 y1 y2] ((juxt :x1 :x2 :y1 :y2) angular-point)
        x-points      (end-include-range x1 x2)
        y-points      (end-include-range y1 y2)]
    (for [x x-points
          y y-points] {:x x
                       :y y})))


(defn calculate-manhattan-distance
  "좌표의 맨하탄 거리 계산"
  [coordinate1 coordinate2]
  (+ (Math/abs (- (:x coordinate1) (:x coordinate2)))
     (Math/abs (- (:y coordinate1) (:y coordinate2)))))

(defn get-border-line-coordinates
  "가장 끝 테두리 라인의 좌표를 return
   input ({:x 1, :y 1} {:x 1, :y 6} {:x 8, :y 3} {:x 3, :y 4} {:x 5, :y 5} {:x 8, :y 9})
   output ({:x 1, :y 1} {:x1 :y 2} {:x 1 :y 3} ... {:x 6 :y 9} {:x 7 :y 9})
   "
  [coordinates]
  (let [angular-point (get-angular-point coordinates)
        [x1 x2 y1 y2] ((juxt :x1 :x2 :y1 :y2) angular-point)
        x-points      (end-include-range x1 x2)
        y-points      (end-include-range y1 y2)]
    (distinct (concat (map (fn [y] {:x x1
                                    :y y}) y-points)
                      (map (fn [y] {:x x2
                                    :y y}) y-points)
                      (map (fn [x] {:x x
                                    :y y1}) x-points)
                      (map (fn [x] {:x x
                                    :y y2}) x-points)))))


(defn find-nearest-coordinate
  "- 맨하탄 거리가 가까운순으로 정렬
   - 첫번째와 두번째 맨하탄 거리 비교후 같으 '.'(동시도착) 이므로 pass 
   - 같지 않으면 더 가까운 좌표 return
   input [{:x 1 :y 1} [{:coordinate {:x 8, :y 9} {:coordinate {:x 3, :y 4} {:coordinate {:x 2, :y 3}]]
   output {:coordinate {:x 2, :y 3}
   "
  [coordinate targets]
  (let [add-distances       (map (fn [v] (into v [{:distance (calculate-manhattan-distance coordinate (:coordinate v))}])) targets)
        sorted-by-distances (sort-by :distance add-distances)]
    (when (not= (calculate-manhattan-distance coordinate (:coordinate (first sorted-by-distances)))
                (calculate-manhattan-distance coordinate (:coordinate (second sorted-by-distances))))
      (apply dissoc (first sorted-by-distances) [:distance]))))


(defn add-near-coordinate
  "영역에 맨하탄거리상 가장 가깝고 충돌(동시 도착)이 없는 좌표들을 :near-coodinate에 추가한다
   input [{:coordinate {:x 3, :y 4}, :near-coordinate []} {:x 1, :y 2}]
   output 
   조건에 만족하지 않는 경우 {:coordinate {:x 3, :y 4}, :near-coordinate []}
   조건에 만족하는경우 {:coordinate {:x 3, :y 4}, :near-coordinate [{:x 1, :y 2}]}
   "
  [target-coordinates coordinate]
  (let [nearest-target (find-nearest-coordinate coordinate target-coordinates)]
    (if nearest-target
      (update-in target-coordinates
                 [(.indexOf target-coordinates nearest-target) :near-coordinate]
                 (fn [near-coordinate-v] (conj near-coordinate-v coordinate)))
      target-coordinates)))


(defn include-border-line?
  "테두리라인 좌표들에 포함 여부"
  [border-line-coordinate near-coordinates]
  (seq (st/intersection (set near-coordinates)
                        (set border-line-coordinate))))


(defn mapping-coordinate
  "문자열로 전달받은 좌표를 hashmap으로 parsing하여 return
   input 1, 1
   output {:x 1 :y 1}
   "
  [coordinate-string]
  (let [[_ & coordinates] (re-matches #"(\d+), (\d+)" coordinate-string)]
    {:x (Integer. (first coordinates))
     :y (Integer. (last coordinates))}))

(defn remove-infinite-increase-coordinate [border-line-coordinates target-coordinates]
  (remove #(include-border-line? border-line-coordinates (:near-coordinate %)) target-coordinates))


;; input 들로 좌표 생성
;; 좌표와 인접한거리의 좌표목록 구조 생성
;; 좌표내의 모든 좌표 생성
;; 좌표들의 테두리라인 좌표 생성
;; 모든 좌표들을 맨하탄거리계산으로 다른좌표와 충돌나지않으면서 가까운 좌표를 가까운좌표목록에 추가
;; 테두리라인에 닿은 좌표가 포함된 타겟들을 제거
;; 카운트 후 최대값 추출
(defn part1
  [input-data]
  (let [coordinates                            (map mapping-coordinate input-data)
        target-coordinate-and-near-coordinates (map (fn [coordinate] {:coordinate      coordinate
                                                                      :near-coordinate []})
                                                    coordinates)
        border-line-coordinates                (get-border-line-coordinates coordinates)]
    (->> (get-all-coordinates coordinates)
         (reduce add-near-coordinate (vec target-coordinate-and-near-coordinates))
         (remove-infinite-increase-coordinate border-line-coordinates)
         (map #(count (:near-coordinate %)))
         (apply max))))

(defn sum-distance-under-10000?
  "타겟 좌표와 나머지 좌표들의 맨하탄 거리 총합이 10000 이하 여부  "
  [target coordinates]
  (let [distances      (map (fn [coordinate] (calculate-manhattan-distance target coordinate)) coordinates)
        total-distance (apply + distances)]
    (< total-distance 10000)))

(defn part2
  [input-data]
  (let [coordinates                            (map mapping-coordinate input-data)]
    (->> (get-all-coordinates coordinates)
         (filter #(sum-distance-under-10000? % coordinates))
         count)))


(comment
  (end-include-range 1 10)
  (get-all-coordinates (seq '([1 1] [1 6] [8 3] [3 4] [5 5] [8 9] [7 10]))))

(comment
  (get-angular-point [({:x 1
                        :y 1} {:x 1
                               :y 6} {:x 6
                                      :y 10})])
  (get-input))

(comment
  (->> (get-input)
       (map mapping-coordinate)
       (get-all-coordinates)))

(comment
  (->> (get-input)
       (map mapping-coordinate)
       (get-border-line-coordinates)))

(comment
  (time (part1 (get-input))))



(comment
  (time (part2 (get-input))))




