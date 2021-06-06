(ns second_day
  (:require [clojure.data]
            [utils]))

(def file-name "2day_part1_input.txt")

; part 1 solution

(defn exists? [m x]
  "[map integer]
    map의 value 중에서 x로 넘겨 받은 값이 존재한다면 true 를 반환합니다.
  "
  (not (empty? (filter (fn [[_ v]] (= v x)) m))))
(defn identity-counter [data]
  "[data]
    data에서 truthy 값만 세어 반환합니다."
  (count (filter identity data)))

(*
  (->> (utils/read-lines file-name frequencies)
       (map #(exists? % 2))
       (identity-counter))
  (->> (utils/read-lines file-name frequencies)
       (map #(exists? % 3))
       (identity-counter)))

;part 2 solution

(defn compare-string [orig-string dest-string]
  (map (fn [orig-string dest-string] (= orig-string dest-string)) orig-string dest-string))

(defn get-correct-box [orig-string dest-string]
  (= (count (filter identity (compare-string orig-string dest-string))) (dec (count orig-string))))

(defn convert-int [bool-list]
  (map {false 0 true 1} bool-list))

(defn remove-different-character [s1 s2]
  (let [index (.indexOf (convert-int (compare-string s1 s2)) 0)]
    (apply str (subs s1 0 index) (subs s1 (inc index)))))


(loop [data (utils/read-lines file-name seq)]
  (let [correct-box (filter (fn [x] (get-correct-box (first data) x)) (rest data))]
    (if (not (empty? correct-box))
      (println
        (remove-different-character (apply str (first data)) (apply str (first correct-box)))))
    (if (not (empty? data))
      (recur (rest data)))))

;refactor
;part1
(defn parse-first-part [data]
  (->> (frequencies data)
       vals
       set))

(defn process-first-part [data]
  (letfn [(sum-only-value-exists [data value] (reduce + (map (fn [x] (if (x value) 1 0)) data)))]
    (*
      (sum-only-value-exists data 2)
      (sum-only-value-exists data 3))))

(->> (utils/read-lines file-name seq)
     (map parse-first-part)
     process-first-part)

;part2

(defn get-only-one-different-character [orig-string dest-string]
  "
  Example:
  (get-only-one-different-character \"ddd\" \"add\")
  => [\"ddd\" \"add\"]
  (get-only-one-different-character \"dsd\" \"add\")
  => nil
  "
  (let [diff (->> (map seq [orig-string dest-string])
                  (apply clojure.data/diff)
                  frequencies)
        diff-words (->> (last diff)
                        first
                        frequencies)]
    (if (and (= (get diff-words nil) 1) (= (count (first (last diff))) 26))
      [orig-string dest-string]
      nil)))

(defn get-only-same-character [orig-string dest-string]
  (->> (map seq [orig-string dest-string])
       (apply clojure.data/diff)
       last
       (filter identity)
       clojure.string/join))

(defn process-part-two [data]
  (loop [[first-ele & rest-eles] data]
    (let [result (->> (map
                        (fn [x] (get-only-one-different-character first-ele x))
                        rest-eles)
                      (filter identity)
                      first)]
      (if (and (seq rest-eles) (empty? result))
        (recur rest-eles)
        (apply get-only-same-character result)))))

(comment (->> (utils/read-lines file-name str)
              process-part-two))