(ns fourth-day
  (:require [clojure.string]
            [clj-time.core]
            [clj-time.coerce]
            [clj-time.format]
            [utils]))

(def file-name "fourthday_input")

(def date-time-formatter (clj-time.format/formatter "yyyy-MM-dd HH:mm"))
(defn parse-datetime [dt-string]
  "[dt-string string]
  입력받은 dt-string을 yyyy-MM-dd HH:mm format으로 datetime instance 화하여 변환

  Example:
  (parse-datetime \"1518-05-04 23:56\")
  => #clj-time/date-time\"1518-05-04T23:56:00.000Z\"
  "
  (clj-time.format/parse date-time-formatter dt-string))

(defn parse-string [message]
  "[message string]

  Example:
  (parse \"[1518-05-04 23:56] Guard #523 begins shift\")
  => [#clj-time/date-time\"1518-05-04T23:56:00.000Z\" \"Guard #523 begins shift\"]
  "
  (let [[date-time behavior] (rest (re-find #"\[(.+)\] (.+)" message))]
    [(parse-datetime date-time) behavior]))

(comment (parse-string "[1518-05-04 23:56] Guard #523 begins shift"))

(defn partition-by-guard-and-behavior [data]
  "
  [([#clj-time/date-time\"1518-01-28T00:00:00.000Z\" \"Guard #151 begins shift\"]
    [#clj-time/date-time\"1518-01-28T00:40:00.000Z\" \"falls asleep\"]
    [#clj-time/date-time\"1518-01-28T00:49:00.000Z\" \"wakes up\"]
    ...) data]

  위와 같이 입력된 데이터를 guard line / 이하 guard line 이 나오기전까지 partition으로 자릅니다.

  Example:
  => (([#clj-time/date-time\"1518-01-28T00:00:00.000Z\" \"Guard #151 begins shift\"])
      ([#clj-time/date-time\"1518-01-28T00:40:00.000Z\" \"falls asleep\"]
       [#clj-time/date-time\"1518-01-28T00:49:00.000Z\" \"wakes up\"]
       [#clj-time/date-time\"1518-01-28T00:57:00.000Z\" \"falls asleep\"]
       [#clj-time/date-time\"1518-01-28T00:58:00.000Z\" \"wakes up\"])
      ([#clj-time/date-time\"1518-01-29T00:04:00.000Z\" \"Guard #2017 begins shift\"])
      ([#clj-time/date-time\"1518-01-29T00:08:00.000Z\" \"falls asleep\"]
       [#clj-time/date-time\"1518-01-29T00:30:00.000Z\" \"wakes up\"]
       [#clj-time/date-time\"1518-01-29T00:42:00.000Z\" \"falls asleep\"]
       [#clj-time/date-time\"1518-01-29T00:53:00.000Z\" \"wakes up\"]))
  "
  (partition-by
    (fn [[_ x]] (clojure.string/includes? x "#"))
    data))

(comment (partition-by-guard-and-behavior (map parse-string (take 10 (sort (utils/read-lines file-name str))))))

(defn get-guard-id [[_ message]]
  "[ANY message] 로 이뤄진 element로부터 message의 digit를 추출하여 반환

  Example:
  (get-guard-id [#object[org.joda.time.DateTime 0x3f4c6ca9 \"1518-01-28T00:00:00.000Z\"] \"Guard #151 begins shift\"])
  => 151
  "
  (Integer/parseInt (first (re-seq #"\d+" message))))

(comment (get-guard-id [1 "Guard #151 begins shift"]))

(defn get-pair-asleep-and-awake [data]
  "
  fall asleep 라인과 wakes up 라인으로 만들어진 list 를 입력받아 각 라인의 dt를 pair로 묶어 줍니다.

  Example:
  (get-pair-asleep-and-awake [[#clj-time/date-time\"1518-01-28T00:40:00.000Z\" \"falls asleep\"]
                              [#clj-time/date-time\"1518-01-28T00:49:00.000Z\" \"wakes up\"]
                              [#clj-time/date-time\"1518-01-28T00:57:00.000Z\" \"falls asleep\"]
                              [#clj-time/date-time\"1518-01-28T00:58:00.000Z\" \"wakes up\"]]
  => ((#clj-time/date-time\"1518-01-28T00:40:00.000Z\" #clj-time/date-time\"1518-01-28T00:49:00.000Z\")
      (#clj-time/date-time\"1518-01-28T00:57:00.000Z\" #clj-time/date-time\"1518-01-28T00:58:00.000Z\"))
  "
  (partition 2 (map (fn [[dt _]] dt) data)))

(comment (get-pair-asleep-and-awake [[1 "foo"] [2 "bar"]]))

(defn get-guard-behavior-map [data]
  "
  guard와 asleep-awake list 로 partition 된 list를 {guard-id asleep-awake pairs} 로 만들어줍니다.

  Example:
  (get-guard-behavior-map [[#clj-time/date-time\"1518-01-28T00:00:00.000Z\" \"Guard #151 begins shift\"]
                           [#clj-time/date-time\"1518-01-28T00:40:00.000Z\" \"falls asleep\"]
                           ...])
  => {2393 ((#clj-time/date-time\"1518-02-22T00:32:00.000Z\" #clj-time/date-time\"1518-02-22T00:44:00.000Z\")
            (#clj-time/date-time\"1518-03-22T00:29:00.000Z\" #clj-time/date-time\"1518-03-22T00:34:00.000Z\")
            (#clj-time/date-time\"1518-03-22T00:47:00.000Z\" #clj-time/date-time\"1518-03-22T00:50:00.000Z\")
            ...)}
  "
  (loop [guard-behavior-map {}
         [first-ele second-ele & rest-eles] data]
    (let [guard-id (get-guard-id (first first-ele))
          behaviors (get guard-behavior-map guard-id)
          pairs-asleep-and-awake (get-pair-asleep-and-awake second-ele)
          updated-guard-behavior-map (cond-> guard-behavior-map
                                             (empty? behaviors) (assoc guard-id pairs-asleep-and-awake)
                                             (not (empty? behaviors)) (assoc guard-id (concat behaviors pairs-asleep-and-awake)))]
      (if (seq rest-eles)
        (recur updated-guard-behavior-map rest-eles)
        updated-guard-behavior-map))))

(defn parse [data]
  (->> data
       sort
       (map parse-string)
       partition-by-guard-and-behavior
       get-guard-behavior-map))

(comment (->> (utils/read-lines file-name str)
              parse))

(defn get-interval-as-minutes [start-dt end-dt]
  "[start-dt org.joda.time.DateTime
    end-dt org.joda.time.DateTime]
  start-dt 와 end-dt 사이의 interval을 minute인 integer로 반환

  Example:
  (get-interval-as-minutes
    #clj-time/date-time\"1518-01-28T00:40:00.000Z\"
    #clj-time/date-time\"1518-01-28T00:49:00.000Z\")
  => 9
  "
  (clj-time.core/in-minutes (clj-time.core/interval start-dt end-dt)))

(comment (get-interval-as-minutes
           (clj-time.core/date-time 2021 1 1 10)
           (clj-time.core/date-time 2021 1 1 10 30)))


(defn sum-of-sleeping-times [data]
  "
  data value의 각 pair 의 dt 들의 interval을 구하여 모두 더합니다.

  Example:
  (sum-of-sleeping-times [[(clj-time.core/date-time 2021 1 1 10) (clj-time.core/date-time 2021 1 1 10 3)]
                          [(clj-time.core/date-time 2021 1 1 11 3) (clj-time.core/date-time 2021 1 1 11 33)]])
  => 33
  "
  (reduce + (map (fn [[x xs]] (get-interval-as-minutes x xs)) data)))

(comment (sum-of-sleeping-times [[(clj-time.core/date-time 2021 1 1 10) (clj-time.core/date-time 2021 1 1 10 3)]
                                 [(clj-time.core/date-time 2021 1 1 11 3) (clj-time.core/date-time 2021 1 1 11 33)]]))

(defn sum-of-sleeping-times-for-each-guard [data]
  "
  hash-map 으로 이루어진 data의 value를 모두 minute interval로 변환합니다.

  Example:
  (sum-of-sleeping-times-for-each-guard {1 [[(clj-time.core/date-time 2021 1 1 10) (clj-time.core/date-time 2021 1 1 10 30)]
                                            [(clj-time.core/date-time 2021 1 1 11 1) (clj-time.core/date-time 2021 1 1 11 3)]]
                                         2 [[(clj-time.core/date-time 2021 1 1 11) (clj-time.core/date-time 2021 1 1 11 5)]]})
  => {1 32, 2 5}
  "
  (reduce merge (map
                  (fn [[guard-id pairs]]
                    {guard-id (sum-of-sleeping-times pairs)})
                  data)))

(comment (sum-of-sleeping-times-for-each-guard {1 [[(clj-time.core/date-time 2021 1 1 10)
                                                    (clj-time.core/date-time 2021 1 1 10 30)]
                                                   [(clj-time.core/date-time 2021 1 1 11 1)
                                                    (clj-time.core/date-time 2021 1 1 11 3)]]
                                                2    [[(clj-time.core/date-time 2021 1 1 11)
                                                       (clj-time.core/date-time 2021 1 1 11 5)]]}))


(defn get-pair-that-has-the-biggest-value [data]
  "
  hash-map에서 가장 큰 value를 가진 key-value pair를 반환합니다.

  Example:
  (get-pair-that-has-the-biggest-value {1 10
                                        2 4
                                        3 5})
  => [1 10]
  "
  (let [max-value (apply max (vals data))]
    (first (filter (fn [[_ x]] (= max-value x)) data))))

(comment (get-pair-that-has-the-biggest-value {1 10
                                               2 4
                                               3 5}))


(comment (->> (utils/read-lines file-name str)
              parse
              sum-of-sleeping-times-for-each-guard
              get-pair-that-has-the-biggest-value))


(defn convert-sleeping-period-to-minute-list [start-dt end-dt]
  "
  start-dt 에서 end-dt 까지 minute만으로 구성된 list를 반환합니다.

  Example:
  (convert-sleeping-period-to-minute-list (clj-time.core/date-time 2021 1 1 10)
                                          (clj-time.core/date-time 2021 1 1 10 5))
  => (0 1 2 3 4)
  "
  (for [x (range (clj-time.core/minute start-dt) (clj-time.core/minute end-dt))] x))

(comment (convert-sleeping-period-to-minute-list (clj-time.core/date-time 2021 1 1 10)
                                                 (clj-time.core/date-time 2021 1 1 10 5)))

(defn get-the-frequency-minutes
  [guard-id data] (->> (get data guard-id)
                       (map #(apply convert-sleeping-period-to-minute-list %))
                       flatten
                       frequencies
                       get-pair-that-has-the-biggest-value
                       first))

(comment (get (->> (utils/read-lines file-name str)
                   parse) 409))


(comment (->> (utils/read-lines file-name str)
              parse
              (get-the-frequency-minutes 409)))

(defn process-first-part [data]
  (let [[the-sleepiest-guard-id _] (->> data
                                        sum-of-sleeping-times-for-each-guard
                                        get-pair-that-has-the-biggest-value)
        the-frequency-minute (->> data
                                  (get-the-frequency-minutes the-sleepiest-guard-id))]
    (* the-sleepiest-guard-id the-frequency-minute)))

; part1
(comment (->> (utils/read-lines file-name str)
              parse
              process-first-part))

(defn get-pair-the-frequency-minutes-and-count [data]
  "
  asleep-awake pairs list를 받아 그중 가장 빈번하게 등장하는 minute 과 그 count를 반환합니다.

  Example:
  (get-pair-the-frequency-minutes-and-count [[(clj-time.core/date-time 2021 1 1 10) (clj-time.core/date-time 2021 1 1 10 5)]
                                             [(clj-time.core/date-time 2021 1 1 10 3) (clj-time.core/date-time 2021 1 1 10 7)]])
  => [3 2]
  "
  (->> (map #(apply convert-sleeping-period-to-minute-list %) data)
       flatten
       frequencies
       get-pair-that-has-the-biggest-value))

(comment (get-pair-the-frequency-minutes-and-count [[(clj-time.core/date-time 2021 1 1 10) (clj-time.core/date-time 2021 1 1 10 5)]
                                                    [(clj-time.core/date-time 2021 1 1 10 3) (clj-time.core/date-time 2021 1 1 10 7)]]))

(defn parse-for-part-two [data]
  (->> (parse data)
       (map (fn [[guard-id pairs]]
              {guard-id
               (->> pairs
                    get-pair-the-frequency-minutes-and-count)}))
       (reduce merge)))

(comment (parse-for-part-two (utils/read-lines file-name str)))

(defn make-map-of-minute-and-count [data]
  "
  {1 [1 1]
   2 [30 4]
   3 [24 2]
   4 [59 7]
   5 [31 2]}
  와 같은 형태를
  => {1 1, 30 4, 24 2, 59 7, 31 2}
  로 변환합니다.
  "
  (->> (vals data)
       (map (fn [[minute count]] {minute count}))
       (reduce merge)))

(comment (->> {1 [1 1]
               2 [30 4]
               3 [24 2]
               4 [59 7]
               5 [31 2]}
              make-map-of-minute-and-count))
              ;get-pair-that-has-the-biggest-value))

(defn find-key-by-value [target-value data]
  "hash-map에서 target-value와 일치하는 key를 반환합니다."
  (first (filter (fn [[_ v]] (= target-value v)) data)))

(defn process-second-part [data]
  (let [most-frequency-pair (->> data
                                 make-map-of-minute-and-count
                                 get-pair-that-has-the-biggest-value)]
    (letfn [(calc-answer [[guard-id [minute _]]] (* guard-id minute))]
      (->> data
           (find-key-by-value most-frequency-pair)
           calc-answer))))

; part2
(comment (->> (utils/read-lines file-name str)
              parse-for-part-two
              process-second-part))
