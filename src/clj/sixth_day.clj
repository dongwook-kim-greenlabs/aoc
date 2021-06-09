(ns sixth_day
  (:require [clojure.string :as str]
            [clojure.set :refer [difference]]
            [utils :refer [read-lines]]))

(def file-name "day6_input")

(defn parse-initial-positions
  "
  Example:
  (parse-initial-positions [\"1, 2\" \"3, 4\" \"5, 5\"])
  => ([1 2] [3 4] [5 5])
  "
  [input]
  (->> (map (fn [x] (str/split x #", ")) input)
       (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)]))))

(defn distance
  "
  start 에서부터 dest 까지의 이동 거리를 반환합니다.
  Example:
  (distance [1 1] [3 3])
  => 4
  "
  [[start-x start-y]
   [dest-x dest-y]]
  (->> [(- start-x dest-x) (- start-y dest-y)]
       (map #(Math/abs ^int %))
       (reduce +)))

(defn get-max-x-y
  "
  입력받은 coll의 element에서 first 최대값 second 최대값을 반환합니다.

  Example:
  (get-max-x-y [[1 1] [2 4] [1 6] [6 1]])
  => [6 6]
  "
  [input]
  [(->> (map (fn [[x _]] [x]) input)
        flatten
        (apply max))
   (->> (map (fn [[_ y]] [y]) input)
        flatten
        (apply max))])

(defn draw
  "
  입력받은 x y 로 이루어진 2차원 seq를 반환합니다.

  Example:
  (draw [3 3])
  => (([0 0] [1 0] [2 0] [3 0])
      ([0 1] [1 1] [2 1] [3 1])
      ([0 2] [1 2] [2 2] [3 2])
      ([0 3] [1 3] [2 3] [3 3]))
  "
  [[max-x max-y]]
  (for [y (range (+ max-y 1))]
    (for [x (range (+ max-x 1))]
      [x y])))

(defn flatten-one-depth-seq
  "
  coll의 element들 끼리 합쳐진 seq를 구성합니다.

  Example:
  (flatten-one-depth-seq [[[1 1]] [[2 1]] [[3 2]]])
  => ([1 1] [2 1] [3 2])"
  [input]
  (reduce concat input))

(defn get-distance-pos-map
  "
  target-pos 에서 각 given-pos element들까지의 거리를 {떨어진 거리 [해당하는 [x y]들]} 으로 반환합니다.
  0인 경우는 제외합니다 (target-pos 와 동일 위치)

  Example:
  (get-distance-pos-map [1 1] [[0 0] [0 1] [0 2] [0 3]
                               [1 0] [1 1] [1 2] [1 3]
                               [2 0] [2 1] [2 2] [2 3]])
  => {2 [[0 0] [0 2] [1 3] [2 0] [2 2]], 1 [[0 1] [1 0] [1 2] [2 1]], 3 [[0 3] [2 3]]}"
  [target-pos given-pos]
  (->> (map (fn [x] [(distance x target-pos) [x]]) given-pos)
       (filter (fn [[distance _]] (> distance 0)))
       (map (fn [x] (apply hash-map x)))
       (apply merge-with into)))

(defn get-the-nearest
  "
  target-pos 로부터 given-pos의 element 중 가장 거리가 가까운 pos의 seq를 반환합니다.

  Example:
  (get-the-nearest [1 1] [[0 0] [0 1] [0 2] [0 3]
                           [1 0] [1 1] [1 2] [1 3]
                           [2 0] [2 1] [2 2] [2 3]])
  => [[0 1] [1 0] [1 2] [2 1]]"
  [target-pos given-pos]
  (let [distance-pos-map (get-distance-pos-map target-pos given-pos)
        min-key (->> (keys distance-pos-map)
                     (apply min))]
    (distance-pos-map min-key)))

(defn parse-part-one [input]
  (let [given-pos (parse-initial-positions input)
        max-x-y-pair (get-max-x-y given-pos)
        grid (draw max-x-y-pair)
        grid-the-nearest-map (->> (flatten-one-depth-seq grid)
                                  (map (fn [x] {x (get-the-nearest x given-pos)}))
                                  (reduce merge))]
    {:given-pos given-pos
     :max-x-y-pair max-x-y-pair
     :grid-the-nearest-map grid-the-nearest-map}))

(defn filter-that-has-finite-boundary
  "
  Example:
  (filter-that-has-finite-boundary
    [[0 0] [3 3] [1 3] [3 1] [2 2]] ; given-pos
    {[2 2] [[3 3] [1 3] [3 1]],
     [0 0] [[1 3] [3 1] [2 2]],
     [1 0] [[0 0]],
     [2 3] [[3 3] [1 3] [2 2]],
     [3 3] [[1 3] [3 1] [2 2]],
     [1 1] [[0 0] [1 3] [3 1] [2 2]],
     [3 0] [[3 1]],
     [1 3] [[3 3] [2 2]],
     [0 3] [[1 3]],
     [0 2] [[0 0] [1 3] [2 2]],
     [2 0] [[0 0] [3 1] [2 2]],
     [3 1] [[3 3] [2 2]],
     [2 1] [[3 1] [2 2]],
     [1 2] [[1 3] [2 2]],
     [3 2] [[3 3] [3 1] [2 2]],
     [0 1] [[0 0]]} ; input (grid-the-nearest-map) {각 grid 의 pos 값 [key의 the nearest position인 given-pos 값들]}
    [3 3]) ; max pos-x pos-y
  => #{[2 2] [3 3]}  ; finite boundary 를 가진 given-pos element"
  [given-pos input [max-x max-y]]
  (letfn [(filter-boundary-only [input] (filter
                                          (fn [[[x y] _]]
                                            (or
                                              (= x 0)
                                              (= y 0)
                                              (= x max-x)
                                              (= y max-y)))
                                          input))
          (filter-only-candidate [input] (filter
                                           (fn [[_ v]]
                                             (< (count v) 2)) input))
          (filter-difference [given-pos input] (->> (vals input)
                                                    flatten-one-depth-seq
                                                    set
                                                    (difference (set given-pos))))]
    (->> (filter-boundary-only input)
         filter-only-candidate
         ((partial filter-difference given-pos)))))

(defn count-how-many-appears
  "
  k가 coll에 얼마나 자주 출현하는지 세서 반환합니다.

  Example:
  (count-how-many-appears [1 1] [[1 1] [1 1] [0 1] [0 0]])
  => 2
  "
  [k coll]
  (->> (filter (fn [x] (= x k)) coll)
       count))

(defn process [{:keys [given-pos max-x-y-pair grid-the-nearest-map]}]
  (let [finite-candidates (filter-that-has-finite-boundary given-pos grid-the-nearest-map max-x-y-pair)
        candidate-appears (->> (for [[k v] grid-the-nearest-map :when (< (count v) 2)] {k v})
                               (reduce merge))
        flatten-candidate-appears-coll (for [[_ [x]] candidate-appears] x)
        counts (mapv #(count-how-many-appears % flatten-candidate-appears-coll) finite-candidates)]
    (apply max counts)))

;part two

(defn parse-part-two [input]
  (let [given-pos (parse-initial-positions input)
        max-x-y-pair (get-max-x-y given-pos)
        grid (draw max-x-y-pair)
        distance-pos-map (->> (flatten-one-depth-seq grid)
                              (map (fn [x] {x (get-distance-pos-map x given-pos)})))]
       (reduce merge distance-pos-map)))

(defn sum-of-all-distance
  "
  int key 와 seq들의 value 로 이루어진 map 을
  (sum (key * (count value)) 을 반환합니다.

  Example:
  (sum-of-all-distance {2 [[1 1]] 3 [[1 2]] 1 [[1 1] [0 1]]})
  => 7
  "
  [coll]
  (->> (map (fn [[k v]] (* k (count v))) coll)
       (reduce +)))

(defn process-part-two [input]
  (->> (map (fn [[k v]] {k (sum-of-all-distance v)}) input)
       (reduce merge)
       (filter (fn [[_ v]] (< v 10000)))
       vals
       count))

(comment
  (parse-initial-positions (read-lines file-name str))

  (->> (read-lines file-name str)
       parse-initial-positions
       get-max-x-y
       draw)

  (->> (parse-part-one (utils/read-lines file-name str)))

  ;part 1
  (->> (utils/read-lines file-name str)
       parse-part-one
       process)

  ;part 2
  (->> (utils/read-lines file-name str)
       parse-part-two
       process-part-two))

