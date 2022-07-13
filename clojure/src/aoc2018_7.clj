(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))


;; 첫문자열 받아서 문자열들을 돌면서 문자가 시작점인것들 가져와서 처리하고 더이상없으면
;; 다음문자열로 또 찾아와서 처리하고 없으면 또 다음문자로 반복처리

(defn input-data []
  (->> "day7.sample.txt"
       (io/resource)
       (slurp)
       (clojure.string/split-lines)
       (map #(rest (re-matches #"Step (\[A-Z]) must be finished before step (\[A-Z]) can begin." %))))
  ;; Step C must be finished before step A can begin.
  )

(defn find-first [input]
  (let [starts (map first input)
        ends   (map last input)]
    (first (sort (st/difference (set starts) (set ends))))))

(defn slice-by-alphabet-sort
  [v]
  (if (<= (count v) 1)
    (count v)
    (loop [fv [(first v)]
           rv (rest v)]
      (if (< (compare (last fv) (first rv)) 0)
        (recur (conj fv (first rv)) (rest rv))
        (.indexOf v (first rv))))))

(defn add-by-index [v c idx]
  (let [s  (subvec v 0 (inc idx))
        r  (subvec v (inc idx) (count v))
        r1 (subvec r 0 (slice-by-alphabet-sort r))
        r2 (subvec r (slice-by-alphabet-sort r))]
    (into [] (concat s (sort (conj r1 c)) r2))))

(defn find-last [input]
  (->> (group-by first input)
       (map (fn [v] {:front (key v)
                     :back  (sort (map last (val v)))}))
       (sort-by (fn [v] (count (:back v))))
       first
       :back
       first))

(defn find-last-key [input-data]
  (let [keys (map first input-data)
        vals (map last input-data)]
    (first (st/difference (set keys)))))

(defn sort-sleigh-assembly [first-assembly data]
  (loop [v    [first-assembly]
         data data
         cnt  0]
    (if (and (< cnt 400) (seq data))
      (let [f (first data)]
        (if (not (seq v))
          (recur [(:front f) (:back f)] (rest data) (inc cnt))
          (let [idx (.indexOf v (:front f))]
            (if (not= idx -1)
              (recur (add-by-index v (:back f) idx) (rest data) (inc cnt))
              (recur v (concat (rest data) [f]) (inc cnt))))))
      v)))


(defn part1 [input-data]
  (let [first-assembly (find-first input-data)
        last-key       (find-last-key input-data)]
    (->> input-data
         (group-by last)
         (map (fn [v] {:back  (key v)
                       :front (last (sort (map first (val v))))}))

         (sort-sleigh-assembly first-assembly))))


(comment
  (part1 (input-data)))

(comment
  [(first [1 2])]
  (subvec ["F" "E"] 0 1)
  (slice-by-alphabet-sort ["F" "E"])
  (slice-by-alphabet-sort ["C"])
  (compare "F" "E")
  (subvec [1 2 3] 1)
  (add-by-index "abde" "c" 2)
  (concat [{:k 1
            :v 1} {:k 2
                   :v 2}] [{:k 3
                            :v 3}]))

(comment
  (->> (input-data)
       (find-last)))

(comment
  (->> (input-data)
       #_(group-by last)
       #_(map (fn [v] {:back  (key v)
                       :front (last (sort (map first (val v))))}))
       (find-last-key)
       #_(reduce (fn [acc v]
                   (if (not (seq acc))
                     [(:v v) (:k v)] Ç
                     (let [idx (.indexOf acc (:v v))]
                       (if (not= idx -1)
                         (add-by-index acc (:k v) idx)
                         acc)))) [])
       #_s/join))

(defn get-depth [obj key]
  (let [depth ((keyword key) obj)]
    (when (depth)
      (val depth))))

;; (defn update-depth [obj pair]
;;   (let [before-depth (get-depth obj (first pair))
;;         depth        (get-depth obj (last pair))]
;;     (if)))

;; (defn part1_2 [input-data]
;;   (let [first-word (find-first input-data)]
;;     (->> input-data
;;          (reduce (fn [acc v]) {(keyword first-word) 0}))))
