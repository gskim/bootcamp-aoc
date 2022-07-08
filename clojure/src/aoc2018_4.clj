(ns aoc2018_4
  (:require (clojure [string :as s])))

(def sample "resources/day4.txt")

(def input (->> (slurp sample)
                s/split-lines))

(defn is-guard-info
  "desription 내용으로 guard 교대 정보인지 확인"
  [description]
  (s/starts-with? description "G"))

(defn get-guard-id
  "description에서 guard ID를 parsing해온다"
  [description]
  (->> (re-matches #"Guard #(\d+) begins shift" description)
       last
       Integer.))

(defn mapping-data
  "전달받은 정보들로 hash-map 재가공하여 return
   id 에는 [date hour min] 으로 PK값을 만듬
   "
  [[date hour min description]]
  (let [is-guard? (is-guard-info description)]
    {:id       (Long. (str (s/replace date #"-" "") hour min))
     :min      min
     :type     (if is-guard? "guard" "time")
     :guard-id (if is-guard? (get-guard-id description) nil)}))

(defn redefine-data
  "input string을 정규식으로 parsing후 필요한 정보들로 hash-map 가공"
  [string]
  (-> (re-matches #"\[(\d+-\d+-\d+) (\d+)\:(\d+)] (.*)" string)
      next
      mapping-data))

(defn order-by-id-asc
  "id(PK) 값으로 오름차순 정렬"
  [mapping-data-list]
  (sort-by :id mapping-data-list))

(defn all-min-by-min-range
  "시작(minute)분과 종료분(minute)까지의 모든 분(minute)들을 return"
  [[start-min end-min]]
  (range (Integer. (:min start-min)) (Integer. (:min end-min))))

(defn get-all-sleep-mins
  "2개씩 (시작분, 종료분) 분할후 각 시작분과 종료분까지의 모든분으로 변환후 return
   input [10 12 20 23]
   output [[10 11] [20 21 22 23]]
   "
  [times]
  (->> times
       (partition-all 2)
       (map all-min-by-min-range)))

(defn make-min-group-by-guard-id
  "guard-id 를 기준으로 분(minute)들을 그룹화하여 전달"
  [partition-list]
  (map (fn [[guard times]]
         {:guard-id (:guard-id (last guard))
          :times    (get-all-sleep-mins times)}) partition-list))

(defn time-list-group-by-guard-id [mapping-data-list]
  (->> (partition-by #(= "guard" (:type %)) mapping-data-list)
       (partition-all 2)
       (make-min-group-by-guard-id)))

(defn merge-data
  "guard-id가 같은 분(minute)들의 데이터를 merge 처리"
  [min-guard-data-list]
  (->> min-guard-data-list
       (group-by :guard-id)
       vals
       (map #(->> {:guard-id (:guard-id (first %))
                   :times    (mapcat :times %)}))))

(defn assoc-total-sleep-minute
  "총 잠든시간을 추가"
  [data]
  (assoc data :sleep-cnt (reduce (fn [acc minutes] (+ acc (count minutes))) 0 (:times data))))

(defn guard-who-sleeps-the-most
  "가장 잠든 총 시간이 많은 가드 정보를 return"
  [data-list]
  (first (sort-by :sleep-cnt > data-list)))

(defn guard-who-fequently-sleep-the-most
  "가장 잠든 분(minute)의 빈도가 많은 가드 정보를 return"
  [data-list]
  (last (sort-by :cnt data-list)))

(defn parse-minute-count
  "분값과 횟수값 keyword로 재가공"
  [[minute cnt]]
  {:minute minute
   :cnt    cnt})

(defn most-frequency-minute-count
  "가장 잠든 빈도가 많은 분과 횟수를 return"
  [data]
  (->> (flatten (:times data))
       frequencies
       (sort-by val)
       last
       (parse-minute-count)))

(defn assoc-most-frequency-minute-and-count
  "가장많이 잠든 빈도가 많은 분과 횟수를 전달받은 data에 merge 처리"
  [data]
  (merge data (most-frequency-minute-count data)))

(defn reprocessing-input
  "읽어드린 input 재가공 처리"
  [input]
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
