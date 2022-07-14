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

(defn update-not-begin-data [done-target not-begin-data]
  (map (fn [v]
         (update v :waiting-targets #(remove (fn [x] (= x done-target)) %))) not-begin-data))

(defn get-ready-targets [not-begin-data]
  (->> not-begin-data
       (filter (fn [v] (empty? (:waiting-targets v))))
       (map :target)
       vec))

(defn init-workers [num]
  (map (fn [n]
         {:worker-num     n
          :target         nil
          :remain-seconds 0}) (range num)))

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
  ;; 1씩 초를 감소시키고
  ;; 초가 0이 된 워커들을 뽑아내고
  ;; 
  [workers waiting-targets done not-begin-data]
  (let [dec-second-workers      (map #(update % :remain-seconds (fn [remain-seconds] (if (= 0 remain-seconds) 0 (- remain-seconds 1)))) workers)
        done-workers            (->> dec-second-workers
                                     (filter #(= (:remain-seconds %) 0)))
        done-targets            (->> done-workers (map :target) (filter #(not (nil? %))) sort)
        updated-not-begin-data  (reduce (fn [acc v] (update-not-begin-data v acc)) not-begin-data done-targets)
        ready-targets           (get-ready-targets updated-not-begin-data)
        filtered-not-begin-data (filter (fn [v] (not (some #{(:target v)} ready-targets))) updated-not-begin-data)
        not-begin-data          filtered-not-begin-data
        waiting-targets         (sort (concat ready-targets waiting-targets))
        updated-done            (distinct (concat done done-targets))
        usable-worker-nums      (->> done-workers
                                     (map :worker-num))]
    (reduce (fn [workers-waiting-targets usable-worker-num]
              (let [workers         (vec (:workers workers-waiting-targets))
                    waiting-targets (:waiting-targets workers-waiting-targets)]
                (if (seq waiting-targets)
                  {:not-begin-data  not-begin-data
                   :done            updated-done
                   :workers         (update-in
                                     workers
                                     [usable-worker-num]
                                     assoc
                                     :target (first waiting-targets)
                                     :remain-seconds (get-second (first waiting-targets)))
                   :waiting-targets (rest waiting-targets)}
                  workers-waiting-targets))) {:not-begin-data  not-begin-data
                                              :done            updated-done
                                              :workers         dec-second-workers
                                              :waiting-targets waiting-targets} usable-worker-nums)))

(defn calculator-order [parsed-input-data]
  (reduce (fn [acc _]
            (let [[not-begin-data waiting done workers sec] ((juxt :not-begin-data :waiting :done :workers :sec) acc)
                  workers-waiting-targets                   (update-worker workers waiting done not-begin-data)
                  waiting-targets                           (:waiting-targets workers-waiting-targets)
                  workers                                   (:workers workers-waiting-targets)
                  done                                      (:done workers-waiting-targets)
                  not-begin-data                            (:not-begin-data workers-waiting-targets)]
              (when (and (empty? not-begin-data) (= (count (filter (fn [w] (= (:remain-seconds w) 0)) workers)) 5))
                (println acc)
                (println workers-waiting-targets))

              (if (and (empty? not-begin-data) (= (count (filter (fn [w] (= (:remain-seconds w) 0)) workers)) 5))
                (reduced (assoc workers-waiting-targets :sec sec))
                {:not-begin-data not-begin-data
                 :waiting        waiting-targets
                 :done           done
                 :workers        workers
                 :sec            (inc sec)}))) {:not-begin-data (group-by-last-target parsed-input-data)
                                                :waiting        []
                                                :done           []
                                                :workers        (init-workers 5)
                                                :sec            0} (range 20000)))

(defn stop-iterator? [v]
  (let [[not-begin-data workers] ((juxt :not-begin-data :workers) v)]
    (take-while (if (and (empty? not-begin-data) (= (count (filter (fn [w] (= (:remain-seconds w) 0)) workers)) 5)) v))))
(defn get-result [v]
  (:result v))

(defn calculator-order-iterator [parsed-input-data]
  (->> (iterate (fn [acc]
                  (let [[group-by-last target waiting result] ((juxt :group-by-last :target :waiting :result) acc)
                        update-data                           (update-not-begin-data target group-by-last)
                        new-wating                            (sort (distinct (concat (:ready-targets update-data) waiting)))]
                    {:group-by-last (:update-group update-data)
                     :target        (first new-wating)
                     :waiting       (rest new-wating)
                     :result        (conj result (first new-wating))})) {:group-by-last (group-by-last-target parsed-input-data)
                                                                         :target        nil
                                                                         :waiting       []
                                                                         :result        []})
       stop-iterator?))

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
  (->> (input-data)
       (calculator-order-iterator)))

(comment
  "part1"
  (->> (input-data)
       calculator-order
       :done
       s/join))


(comment
  "part2"
  (->> (input-data)
       calculator-order
       :sec))

