(ns seventh_day
  (:require [clojure.set :refer [difference]]
            [clojure.string :as str]
            [flatland.ordered.map :refer [ordered-map]]
            [flatland.ordered.set :refer [ordered-set]]
            [utils :refer [read-lines]]))

(def file-name "day7_input")

(defn parse-orig-dest [input]
  (->> (re-find #"Step ([A-Z]) .* step ([A-Z])" input)
       rest))

(defn get-orig-dest-pair
  "
  Example:
  (get-orig-dest-pair \"Step C must be finished before step A can begin.\")
  => #ordered/map([\"C\" [\"A\"]])
  "
  [input]
  (let [pair (parse-orig-dest input)]
    (ordered-map (first pair) [(second pair)])))

(defn get-dest-orig-pair
  "
  Example:
  (get-dest-orig-pair \"Step C must be finished before step A can begin.\")
  => #ordered/map([\"A\" [\"C\"]])
  "
  [input]
  (let [pair (parse-orig-dest input)]
    (ordered-map (second pair) [(first pair)])))

(defn find-root
  "
  가능한 모든 root를 sort 하여 보여줍니다.

  Example:
  (find-root data-after-parse)
  => (\"C\")"
  [input]

  (let [origins (->> (keys input)
                     set)
        dests (->> (vals input)
                   flatten
                   set)]
    (sort (difference origins dests))))

(defn parse [input]
  (letfn [(make-pair-as-map [x] (->> (map x input)
                                     (apply merge-with concat)))]
    {:orig-dest-pair (make-pair-as-map get-orig-dest-pair)
     :prerequisites-pair (make-pair-as-map get-dest-orig-pair)}))

(defn every-prerequisites?
  "
  prerequisites 의 값들이 coll에 존재할 경우 true 를 반환합니다.

  Example:
  (every-prerequisites? [\"B\" \"C\"] \"ABCE\")
  => true
  (every-prerequisites? [\"D\"] \"ABCE\")
  => false
  "
  [prerequisites coll]

  (every? #(str/includes? coll %) prerequisites))

(defn list-available [prerequisites-pair done-coll available-candidates]
  (filter
    (fn [x] (every-prerequisites? (prerequisites-pair x) (vec done-coll)))
    available-candidates))

(defn process-part-one [{:keys [orig-dest-pair prerequisites-pair]}]
  (let [root (->> (find-root orig-dest-pair)
                  first)
        init-available-dest (concat (orig-dest-pair root) (rest (find-root orig-dest-pair)))
        init-result (ordered-set root)]
    (loop [result init-result
           available-destinations init-available-dest]
      (let [current-pos (->> (list-available prerequisites-pair result available-destinations)
                             first)
            available-dest-by-current-pos (orig-dest-pair current-pos)]
        (if (seq available-destinations)
          (recur
            (conj result current-pos)
            (-> (filter #(not= % current-pos) available-destinations)
                (concat available-dest-by-current-pos)
                sort))
          (str/join "" result))))))

(defn make-as-list
  "
  A: 61, B: 62 ... 로 놓고 value에 해당하는 수만큼의 key 로 구성된 seq를 반환합니다.
  Example:
  (make-as-list \"A\")
  => [\"A\" \"A\" \"A\" ... \"A\"] ;; 60 + 1개
  "
  [ch]

  (let [ascii-digit (int (first ch))
        number-by-char (- ascii-digit 4)]
    (for [_ (range number-by-char)] ch)))

(defn get-idle-worker
  "
  Example:
  (get-idle-worker {:a [] :b [1] :c [] :d [1 2]})
  => (:a :c)"
  [workers]

  (->> (filter (fn [[_ v]] (empty? v)) workers)
       (into {})
       keys))

(defn pop-from-workers
  "
  Example:
  (pop-from-workers {:a [] :b [1] :c [] :d [1 2]})
  => {:a (), :b (), :c (), :d (2)}
  "
  [workers]

  (into {} (map (fn [[k v]] [k (rest v)]) workers)))

(defn push-to-workers
  "
  Example:
  (push-to-idle-workers {:a [] :b [1] :c [] :d [1 2]} [:a :c] [\"A\" \"B\"])
  =>{:a (\"A\" \"A\" \"A\" ...)
     :b [1]
     :c (\"B\" \"B\" \"B\" ...)
     :d [1 2]
  "
  [workers idle-keys tasks]

  (->> (zipmap idle-keys (map make-as-list tasks))
       (merge workers)))

(defn get-available-candidates
  "
  current-pos 에서 도달 가능한 destinations 를 반환합니다.
  current-pos가 orig-dest-pair 에 없을 경우 #{} 를 반환합니다.

  Example:
  (get-available-candidates {\"C\" [\"A\"] \"A\" [\"B\"]} [\"C\"])
  => #{\"A\"}
  (get-available-candidates {\"C\" [\"A\"] \"A\" [\"B\"]} [\"Z\"])
  => #{}
  "
  [orig-dest-pair current-pos]

  (let [result
        (->> (map orig-dest-pair current-pos)
             flatten
             set)]
    (cond
      (= result #{nil}) #{}
      :else result)))

(defn get-next-tasks
  "
  current-pos 에서 accu-done 된 위치를 prerequisites-pair 에서 참조하여
  가능한 destinations 을 orig-dest-pair 에서 가져와 반환합니다.
  Example:
  (get-next-tasks
    {:orig-dest-pair (ordered-map [[\"C\" [\"A\" \"F\"]] [\"A\" [\"B\" \"D\"]] [\"B\" [\"E\"]] [\"D\" [\"E\"]] [\"F\" [\"E\"]]]),
     :prerequisites-pair (ordered-map [[\"A\" [\"C\"]] [\"F\" [\"C\"]] [\"B\" [\"A\"]] [\"D\" [\"A\"]] [\"E\" [\"B\" \"D\" \"F\"]]])}
    [\"C\"]
    [\"C\"])
  => (\"F\" \"A\")
  "
  [{:keys [orig-dest-pair
           prerequisites-pair]}
   accu-done
   current-pos]
  (let [available-candidates (get-available-candidates orig-dest-pair current-pos)]
    (list-available prerequisites-pair accu-done available-candidates)))

(defn get-finish-tasks-on-next
  "
  1개 element 만 남은 pair의 value들을 반환합니다.

  Example:
  (get-finish-tasks-on-next {:a [1] :b [2 2] :c [3 3 3]})
  => (1)
  (get-finish-tasks-on-next {:a [1] :b [2 2] :c [3]})
  => (1 3)
  "
  [workers]
  (->> (filter (fn [[_ v]] (= (count v) 1)) workers)
       (map second)
       flatten))

(defn process-part-two [input]
  (let [init-workers {:worker1 []
                      :worker2 []
                      :worker3 []
                      :worker4 []
                      :worker5 []}]
    (loop [info input
           tasks []
           done []
           timer 0
           workers (push-to-workers init-workers
                                    (get-idle-worker init-workers)
                                    (find-root (input :orig-dest-pair)))]
      (let [current-workers (pop-from-workers workers)
            idle-worker-keys (get-idle-worker current-workers)
            current-done (get-finish-tasks-on-next workers)
            accu-done (concat done current-done)
            next-tasks (get-next-tasks info accu-done current-done)
            current-tasks (sort (concat tasks next-tasks))
            to-do-tasks (take (count idle-worker-keys) current-tasks)
            queued-tasks (->> (map set [current-tasks to-do-tasks])
                              (apply difference))
            next-workers (push-to-workers current-workers idle-worker-keys to-do-tasks)
            next-timer (inc timer)]
        (if (some (fn [[_ v]] (not-empty v)) next-workers)
          (recur
            info
            queued-tasks
            accu-done
            next-timer
            next-workers)
          next-timer)))))


(comment
  (def prerequisites-pair {"A" ["C"]
                           "F" ["C"]
                           "B" ["A"]
                           "D" ["A"]
                           "E" ["B" "D" "F"]})
  (def available-destinations ["A" "F"])

  (filter (fn [x] (every-prerequisites? (prerequisites-pair x) ["C" "A" "B"])) ["E" "D" "F"])

  (def test-data ["Step C must be finished before step A can begin."
                  "Step C must be finished before step F can begin."
                  "Step A must be finished before step B can begin."
                  "Step A must be finished before step D can begin."
                  "Step B must be finished before step E can begin."
                  "Step D must be finished before step E can begin."
                  "Step F must be finished before step E can begin."])

  (get-orig-dest-pair "Step C must be finished before step A can begin.")

  (parse test-data)

  (def data-after-parse (apply merge-with concat (map get-orig-dest-pair test-data)))

  (find-root data-after-parse)

  (->> (read-lines file-name str)
       parse)

  (->> (parse test-data)
       process-part-one)

  ;part1
  (->> (read-lines file-name str)
       parse
       process-part-one)

  ;part2
  (def data-after-parse (parse (read-lines file-name str)))

  (->> (read-lines file-name str)
       parse
       process-part-two))
