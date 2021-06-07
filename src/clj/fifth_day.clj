(ns fifth_day
  (:require [clojure.string :as str]
    [utils]))

(def file-name "day5_input")

(defn get-chars [ascii_start ascii_range]
  (map char (range ascii_start (+ ascii_start ascii_range))))

(def alphabet-count 26)
(def ascii-A 65)
(def ascii-a 97)
(def upper-case-chars (get-chars ascii-A alphabet-count))
(def lower-case-chars (get-chars ascii-a alphabet-count))

(defn make-str-combination-of-collections [[x-coll y-coll]]
  "
  collection 두개를 조합하여 str 으로 만듭니다.

  Example:
  (make-str-combination-of-collections [[1 2 3] [4 5 6]])
  => (\"14\" \"25\" \"36\" \"41\" \"52\" \"63\")
  "
  (->> [(zipmap x-coll y-coll)
        (apply zipmap (reverse [x-coll y-coll]))]
       (reduce merge)
       (map #(apply str %))))

(defn parse-part-one [input]
  (first input))

(defn remove-reactive-character [string coll]
  "
  string 에서 coll의 element를 제거합니다.
  제거할 element가 string에 없다면 nil을 반환합니다.

  Example:
  (remove-reactive-character \"asd\" [\"a\" \"d\"])
  => \"s\"
  (remove-reactive-character \"asd\" [\"e\"])
  => nil
  "
  (letfn [(remove-characters [x xs] (str/replace x xs ""))]
    (let [result (reduce remove-characters string coll)]
      (if (= result string)
        nil
        result))))

(defn remove-recursivly-reactive-character [input]
  "
  string 에서 get-reactive-combo 에서 반환한 coll을 제거합니다.
  제거할 element가 없어 nil을 반환할때까지 반복하여 제거합니다.

  Example:
  (remove-recursivly-reactive-character \"aaAAaa\")
  => \"aa\"
  (remove-recursivly-reactive-character \"aAbBcCdDeEfF\")
  => \"\"
  "
  (loop [input input]
    (let [reactive-combos (make-str-combination-of-collections [upper-case-chars lower-case-chars])
          revised-string (remove-reactive-character input reactive-combos)]
      (if revised-string
        (recur revised-string)
        input))))

(defn process-part-one [input]
  (->> (remove-recursivly-reactive-character input)
       count))

(defn solve-part-one [input]
  (->> (parse-part-one input)
       (process-part-one)))

(comment
  (get-chars ascii-A alphabet-count)

  (make-str-combination-of-collections [upper-case-chars lower-case-chars])

  (remove-reactive-character (first (utils/read-lines file-name read-string)) reactive-combos)

  (solve-part-one (utils/read-lines file-name read-string)))

; part two

(defn remove-each-pair-from-string [input coll]
  "
  Example:
  (remove-each-pair-from-string \"aAbBcCdDeEfF\" [[\"a\" \"A\"]
                                                  [\"b\" \"B\"]])
  => (\"bBcCdDeEfF\" \"aAcCdDeEfF\")"
  (map (fn [x] (remove-reactive-character input x)) coll))

(defn process-part-two [input]
  (let [character-pairs-for-removing (zipmap (map str upper-case-chars) (map str lower-case-chars))]
    (->> (remove-each-pair-from-string input character-pairs-for-removing)
         (map process-part-one)
         (apply min))))

(comment
  (remove-each-pair-from-string
    (first (utils/read-lines file-name read-string))
    (zipmap (map str upper-case-chars) (map str lower-case-chars)))

  (process-part-two (parse-part-one (utils/read-lines file-name read-string))))
