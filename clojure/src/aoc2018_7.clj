(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))

(defn find-first-priority-works
  "가장 첫번째로 작업해야될 work들을 return
   input ([A B] [C A] [C F] [B E] [D E] [F E] [A D])
   output (C)
   "
  [input-data]
  (let [works          (map first input-data)
        priority-works (map last input-data)]
    (sort (st/difference (set works) (set priority-works)))))

(defn group-by-work
  "작업과 우선작업목록들로 grouping 하여 return
   input ([A B] [C A] [C F] [B E] [D E] [F E] [A D])
   output ({:work C, :priority-works []}
           {:work B, :priority-works (A)}
           {:work A, :priority-works (C)}
           {:work F, :priority-works (C)}
           {:work E, :priority-works (B D F)}
           {:work D, :priority-works (A)})
   "
  [parsed-input-data]
  (let [first-works   (->> (find-first-priority-works parsed-input-data)
                           (map (fn [v] {:work           v
                                         :priority-works []})))
        group-by-last (->> parsed-input-data
                           (group-by last)
                           (map (fn [v] {:work           (key v)
                                         :priority-works (sort (map first (val v)))})))]
    (concat first-works group-by-last)))

(defn get-second
  "알파벳별 지정된 초를 return
   input A
   output 61
   input B
   output 62
   "
  [key]
  (+ (- (int (.charAt key 0)) 64) 60))


(defn input-data []
  (->> "day7.sample.txt"
       (io/resource)
       (slurp)
       (clojure.string/split-lines)
       (map (fn [v] (let [[_ first second] (re-matches #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." v)]
                      [first second])))))

(defn get-ready-works
  "남은 작업들중에서 worker로 넣을 준비가 된 (선행작업이 더이상 없는) work 를 추출"
  [remaining-works]
  (->> remaining-works
       (filter (fn [v] (empty? (:priority-works v))))
       (map :work)
       vec))

(defn init-workers
  "전달받은 숫자만큼 worker init return"
  [num]
  (map (fn [n]
         {:worker-num     n
          :work           nil
          :remain-seconds 0}) (range num)))

(defn done? [v]
  (let [[remaining-works workers] ((juxt :remaining-works :workers) v)]
    (not (and (empty? remaining-works) (= (count (filter (fn [w] (zero? (:remain-seconds w))) workers)) (count workers))))))

(defn fill-worker
  "사용가능한 workers에 대기중인(:ready-works) :work를 채워넣고 :ready-works 에서 제거"
  [state]
  (let [usable-workers            (:usable-workers state)
        usable-worker-numbers     (map :worker-num usable-workers)
        worker-num-ready-work-map (into {} (map hash-map usable-worker-numbers (:ready-works state)))]
    (-> state
        (update :workers (fn [workers]
                           (map (fn [worker]
                                  (let [ready-work (get worker-num-ready-work-map (:worker-num worker))]
                                    (if (nil? ready-work)
                                      worker
                                      (assoc worker
                                             :work ready-work
                                             :remain-seconds (get-second ready-work)))))
                                workers)))
        (update :ready-works (fn [ready-works] (subvec ready-works (count worker-num-ready-work-map)))))))

(defn update-ready-works
  "선행작업이 모두 완료된 작업들을 남은작업목록에서 제거, 작업준비 대기열에 추가"
  [state]
  (let [ready-works (get-ready-works (:remaining-works state))]
    (-> state
        (update :remaining-works (fn [remaining-works] (filter (fn [v] (not (some #{(:work v)} ready-works))) remaining-works)))
        (update :ready-works (fn [v] (vec (sort (concat v ready-works))))))))

(defn update-done-work
  "워커에서 완료된 작업을 done 대기열에 추가, 남은 작업의 선행작업 목록에서 완료된 작업 제거"
  [state]
  (let [done-works (->> (:usable-workers state) (map :work) (filter #(not (nil? %))) sort)]
    (-> state
        (update :done #(distinct (concat % done-works)))
        (update :remaining-works (fn [remaining-works]
                                   (map (fn [remaining-work]
                                          (update remaining-work
                                                  :priority-works
                                                  (fn [priority-work]
                                                    (st/difference (set priority-work) (set done-works))))) remaining-works))))))

(defn update-worker-remain-second-descrese
  "워커에 진행중인 작업이 있는경우 1초 감소"
  [state]
  (update state :workers (fn [workers]
                           (map (fn [worker] (update worker :remain-seconds (fn [remain-seconds] (if (zero? remain-seconds) 0 (dec remain-seconds))))) workers))))

(defn update-usable-workers [state]
  (assoc state :usable-workers (filter #(zero? (:remain-seconds %)) (:workers state))))

(defn update-state [state]
  (-> state
      update-worker-remain-second-descrese
      update-usable-workers
      update-done-work
      update-ready-works
      fill-worker
      (update :sec inc)))

(defn init-data [worker-num parsed-input-data]
  {:remaining-works (group-by-work parsed-input-data)
   :usable-workers  []
   :ready-works     []
   :done            []
   :workers         (init-workers worker-num)
   :sec             -1})

(comment
  (map hash-map [1 2 3] ["A" "B"])
  (map hash-map [1] ["A" "B"])
  (update {:remain-seconds 1} :remain-seconds dec)
  (->> (input-data)
       find-first-priority-works)
  (->> (input-data)
       (group-by-work)))

(comment
  "part1"
  (->> (input-data)
       (init-data 5)
       (iterate update-state)
       (drop-while done?)
       first
       :done
       s/join))
(comment
  "part2"
  (->> (input-data)
       (init-data 5)
       (iterate update-state)
       (drop-while done?)
       first
       :sec))

