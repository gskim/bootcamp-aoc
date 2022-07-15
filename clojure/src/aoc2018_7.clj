(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))

(defn find-first-targets [input-data]
  (let [first-targets (map first input-data)
        next-targets  (map last input-data)]
    (sort (st/difference (set first-targets) (set next-targets)))))

(defn group-by-last-target [parsed-input-data]
  (let [first-targets (->> (find-first-targets parsed-input-data)
                           (map (fn [v] {:target          v
                                         :waiting-targets []})))
        group-by-last (->> parsed-input-data
                           (group-by last)
                           (map (fn [v] {:target          (key v)
                                         :waiting-targets (sort (map first (val v)))})))]
    (concat first-targets group-by-last)))

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
  (->> "day7.txt"
       (io/resource)
       (slurp)
       (clojure.string/split-lines)
       (map (fn [v] (let [[_ first second] (re-matches #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." v)]
                      [first second])))))

(defn update-not-begin-data
  "시작하지못한 data에서 완료된 target들을 waiting-targets에서 제거"
  [done-target not-begin-data]
  (map (fn [v]
         (update v :waiting-targets #(remove (fn [x] (= x done-target)) %))) not-begin-data))

(defn get-ready-targets
  "시작하지못한 data에서 worker로 넣을 준비가 된 (:waiting-targets이 더이상 없는) data를 추출"
  [not-begin-data]
  (->> not-begin-data
       (filter (fn [v] (empty? (:waiting-targets v))))
       (map :target)
       vec))

(defn init-workers [num]
  (map (fn [n]
         {:worker-num     n
          :target         nil
          :remain-seconds 0}) (range num)))

(defn remove-done-target-to-not-begin-data [not-begin-data done-targets]
  (reduce (fn [acc v] (update-not-begin-data v acc)) not-begin-data done-targets))

(defn dec-second-to-workers [workers]
  (map #(update % :remain-seconds (fn [remain-seconds] (if (= 0 remain-seconds) 0 (- remain-seconds 1)))) workers))

(defn update-worker
  "worker 개수가 2개인경우
   input [
   {:worker-num 0 :target A :remain-seconds 1}
   {:worker-num 1} :target B :remain-seconds 2
   ]
   [C D]
   [F]
   [{:target C, :waiting-targets [A]}]
   output {
   :workers [
   {:worker-num 0 :target C :remain-seconds 3}
   {:worker-num 1} :target B :remain-seconds 1
   ]
   :waiting-targets [D]
   :done [F A]
   }
   "
  [workers waiting-targets done not-begin-data]
  (let [decreased-second-workers (dec-second-to-workers workers)
        done-workers             (->> decreased-second-workers (filter #(= (:remain-seconds %) 0)))
        done-targets             (->> done-workers (map :target) (filter #(not (nil? %))) sort)
        updated-not-begin-data   (remove-done-target-to-not-begin-data not-begin-data done-targets)
        ready-targets            (get-ready-targets updated-not-begin-data)
        filtered-not-begin-data  (filter (fn [v] (not (some #{(:target v)} ready-targets))) updated-not-begin-data)
        waiting-targets          (sort (concat ready-targets waiting-targets))
        updated-done             (distinct (concat done done-targets))
        usable-worker-nums       (->> done-workers (map :worker-num))]
    (reduce (fn [workers-waiting-targets usable-worker-num]
              (let [workers         (vec (:workers workers-waiting-targets))
                    waiting-targets (:waiting-targets workers-waiting-targets)
                    target          (first waiting-targets)]
                (if (target)
                  {:not-begin-data  filtered-not-begin-data
                   :done            updated-done
                   :workers         (update-in
                                     workers
                                     [usable-worker-num]
                                     assoc
                                     :target target
                                     :remain-seconds (get-second target))
                   :waiting-targets (rest waiting-targets)}
                  workers-waiting-targets)))
            {:not-begin-data  filtered-not-begin-data
             :done            updated-done
             :workers         dec-second-workers
             :waiting-targets waiting-targets}
            usable-worker-nums)))

(defn done? [v]
  (let [[not-begin-data workers] ((juxt :not-begin-data :workers) v)]
    (and (empty? not-begin-data) (= (count (filter (fn [w] (= (:remain-seconds w) 0)) workers)) (count workers)))))


(defn calculator-order-iterator [worker-num parsed-input-data]
  (iterate (fn [acc]
             (let [[not-begin-data waiting done workers sec] ((juxt :not-begin-data :waiting :done :workers :sec) acc)
                   workers-waiting-targets                   (update-worker workers waiting done not-begin-data)]
               (assoc workers-waiting-targets :sec (inc sec))))
           {:not-begin-data (group-by-last-target parsed-input-data)
            :waiting        []
            :done           []
            :workers        (init-workers worker-num)
            :sec            0}))

(comment
  (update {:remain-seconds 1} :remain-seconds dec)
  (update-worker [{:worker-num     0
                   :target         "A"
                   :remain-seconds 1}
                  {:worker-num     1
                   :target         "B"
                   :remain-seconds 2}] ["D" "E"] ["F"]
                 [{:target          "C"
                   :waiting-targets ["A"]} {:target          "G"
                                            :waiting-targets ["A"]}])
  (->> (input-data)
       (group-by-last-target)))

(comment
  "part1"
  (->> (input-data)
       (calculator-order-iterator 1)
       (filter done?)
       first
       :done
       s/join))

(comment
  "part2"
  (->> (input-data)
       (calculator-order-iterator 5)
       (filter done?)
       first
       :sec))

