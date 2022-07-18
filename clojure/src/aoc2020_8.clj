(ns aoc2020_8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-input [input]
  (->> input
       (map (fn [v] (s/split v #" ")))
       vec))

(defn get-input [] (-> "2020_day8.sample.txt" io/resource slurp s/split-lines parse-input))

(defn mapping-by-id
  "input [[nop +0] [acc +1]]
   output {0 {:action :nop :cnt 0} 1 {:action :acc :cnt 1}}
   "
  [parsed-input]
  (->> (map-indexed (fn [idx [action cnt]]
                      [idx {:action (keyword action)
                            :cnt    (read-string cnt)}])
                    parsed-input)
       (into {})))

(defn get-next-id [data before-id]
  (if (nil? before-id)
    0
    (let [before-data (get data before-id)]
      (when before-data
        (case (:action before-data)
          (:nop :acc) (inc before-id)
          :jmp (+ before-id (:cnt before-data)))))))

(defn init-state [data]
  {:data-map        (mapping-by-id data)
   :next-id         nil
   :sum             0
   :reached-last-id false
   :ids             #{}})

(defn duplicate-id? [state]
  (let [next-id      (:next-id state)
        before-ids   (:ids state)
        distinct-ids (conj before-ids next-id)]
    (not= (count before-ids) (count distinct-ids))))

(defn reached-last-id? [state]
  (not (:reached-last-id state)))

(defn update-next-id
  "이전에 사용된 next-id를 사용하여 다음 next-id를 찾아서 update 한다."
  [state]
  (let [[data-map before-id] ((juxt :data-map :next-id) state)
        next-id              (get-next-id data-map before-id)]
    (assoc state :next-id next-id :reached-last-id (= (inc next-id) (count data-map)))))

(defn calc-action
  "state 의 :next-id 로 :data-map에서 가져온 :action :cnt 로 합계를 계산하거나 :ids 를 update 한다."
  [state]
  (let [[data-map next-id] ((juxt :data-map :next-id) state)
        data               (get data-map next-id)
        [action cnt]       ((juxt :action :cnt) data)]
    (if (nil? next-id)
      state
      (-> state
          (update :ids #(conj % next-id))
          (update :sum #(if (= action :acc) (+ % cnt) %))))))

(defn update-state [state]
  (-> state
      calc-action
      update-next-id))

(defn update-data-map-action
  "전달받은 index 위치의 action을 수정한다. jmp -> nop, nop -> jmp, acc -> acc"
  [state idx]
  (update-in state [:data-map idx :action] (fn [action]
                                             (case action
                                               :jmp :nop
                                               :nop :jmp
                                               :acc action))))

(defn get-all-case-states
  "모든 case의 state를 만들고 변경점이없는(acc case) case는 filter 처리"
  [state]
  (->> (for [x (range (count (:data-map state)))]
         (when (not= (:action (get (:data-map state) x)) :acc)
           (update-data-map-action state x)))
       (filter (fn [v] (not (nil? v))))))

(defn get-duplicated-case [state]
  (->> state
       (iterate update-state)
       (drop-while duplicate-id?)
       first))


(defn get-terminated-or-dupliacated-case [state]
  (->> state
       (iterate update-state)
       (drop-while (fn [{:keys [next-id ids reached-last-id]}]
                     (and (not reached-last-id)
                          (not= (count ids) (count (conj ids next-id))))))
       first))

(defn part1
  "part1"
  []
  (->> (get-input)
       (init-state)
       get-duplicated-case
       :sum))

(defn part2
  "part2"
  []
  (->> (get-input)
       init-state
       get-all-case-states
       (map get-terminated-or-dupliacated-case)
       (filter #(:reached-last-id %))
       first
       :sum))

(comment
  (->> (get-input)
       mapping-by-id))

(comment
  (part1)
  (part2))
